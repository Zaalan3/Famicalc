import os

from tkinter import *
from tkinter import ttk
from tkinter.filedialog import *
from tkinter.messagebox import *

import zipfile
from tivars import *
from tivars.types import *
import itertools 

# import ines


class INes: 
	def __init__(self,data):
		self.header = data[0:16]
		self.data = data[16:]
		return
		
	# PRG ROM size in 16kb pages
	def prgsize(self):
		size = self.header[4]
		upper = self.header[9] & 0x0F 
		# don't support PRG sizes greater than 512kb 
		if self.type() == "NES 2.0" and upper != 0:
			return -1 
		return size
		
	# CHR size in 8kb pages
	def chrsize(self):
		size = self.header[5]
		upper = (self.header[9] & 0xF0) >> 4  
		# don't support CHR sizes greater than 512kb 
		if self.type() == "NES 2.0" and upper != 0:
			return -1 
		return size
	
	def mirroring(self): 
		m = self.header[6] & 0b00001001
		if m == 0: 
			return "V"
		elif m == 1: 
			return "H" 
		else:
			return "O"

	def mirroringInt(self): 
		m = self.header[6] & 0b00000001
		if self.header[6] & 0b00001000 != 0: 
			m += 2 
		return m 
		
	def mapper(self):
		mapper = 0 
		if self.type() == "NES 2.0": 
			mapper = ((self.header[8] & 0x0F) << 4) + (self.header[7] & 0xF0) + ((self.header[6] & 0xF0) >> 4)
		else: 
			mapper = ((self.header[6] & 0xF0) >> 4)
			# for old ROM dumps
			if self.header[12:16] == [0,0,0,0]: 
				mapper += (self.header[7] & 0xF0)
		return mapper 
	
	def type(self): 
		if self.header[7] & 0b00001100 == 0b00001000:
			return "NES 2.0" 
		else: 
			return "iNES"
	
	def isValid(self):
		supported_mappers = [0,2]
		# a number of conditions to see if the rom can be transferred to calc.
		
		result = (True,None) 
		if self.prgsize() < 0 or self.prgsize() > 32:
			result = (False,"ROM too large.") 
		elif self.chrsize() < 0 or self.chrsize() > 64:
			result = (False,"ROM too large.")
		elif not(self.mapper() in supported_mappers): 
			result = (False,"Unsupported mapper.") 
		elif len(self.data) != 16384*self.prgsize() + 8192*self.chrsize(): 
			result = (False,"Invalid data size.") 
		elif not(self.header[0:4] == b"NES\x1a"): 
			result = (False,"Invalid ROM header")
		return result
	
	def getHeader(self): 
		return bytes(b"FAMICALC") + bytes([self.mapper(),self.mirroringInt(),self.prgsize(),self.chrsize()])
	
	def getprg(self): 
		prg = self.data[0:16384*self.prgsize()]
		return itertools.batched(prg,16384*3)
	
	def getchr(self): 
		chr = self.data[16384*self.prgsize():]
		return itertools.batched(chr,8192*7)
	
bundle_metadata = { 
	"b83" : ("bundle_identifier:TI Bundle\n"
		"bundle_format_version:1\n"
		"bundle_target_device:83CE\n"
		"bundle_target_type:CUSTOM\n"
		"bundle_comments:N/A\n"), 
	"b84" : ("bundle_identifier:TI Bundle\n"
		"bundle_format_version:1\n"
		"bundle_target_device:84CE\n"
		"bundle_target_type:CUSTOM\n"
		"bundle_comments:N/A\n"), 
}
	
class TIBundle: 
	
	def __init__(self, name: str, *, ext: str = "b84"): 
		self.zip = zipfile.ZipFile(name,'w',allowZip64=False)
		self.ext = ext
		self.zip.writestr("METADATA",bundle_metadata[ext]) 
		
	
	def addData(self, data: bytes, arcname: str):
		#make file to add to zip
		appvar = TIAppVar(name=arcname,data=data)
		self.zip.writestr(arcname+".8xv",appvar.export().bytes())
	
	def writeChecksum(self): 
		checksum = 0
		for info in self.zip.infolist(): 
			checksum += info.CRC
		checksum &= 0xFFFFFFFF
		self.zip.writestr("_CHECKSUM",f"{checksum:x}\r\n")
	
	def close(self):
		self.writeChecksum()
		self.zip.close()


root = Tk() 
root.title("NES to Appvar Utility") 
frame = ttk.Frame(root,padding = (5,10))
frame.grid(column=0,row=0)

rom = None 

var_name = StringVar()
var_description = StringVar()
bundle = StringVar()
rominfo = StringVar()

bundle.set("b84")

def selectFile():  
	global rom
	global rominfo
	
	filename = askopenfilename(title = "Please select a file",filetypes = [("NES ROM files","*.nes")])
	if filename == '': 
		return
	with open(filename,'rb') as f: 
		data = bytes(f.read())
		rom = INes(data)
	
	var_description.set(filename.rsplit("/")[-1].rsplit("\\")[-1].removesuffix(".nes"))
	rominfo.set(f"Header: \n\t{rom.type()}\nMapper: \n\t{rom.mapper()}\nPRG ROM: \n\t{rom.prgsize()*16}KB\nCHR ROM: \n\t{rom.chrsize()*8}KB\nMirroring: \n\t{rom.mirroring()}")
	return 

def convertFile():
	global rom
	global var_name
	global var_description
	global bundle
	if rom is None:
		return 
		
	error = rom.isValid() 
	if not error[0]:
		showerror(message=f"ERROR:\n{error[1]}\n")
		return 
	elif len(var_name.get()) < 1 or len(var_name.get()) > 6 or not var_name.get().isalnum() or not var_name.get()[0].isalpha(): 
		showerror(message="ERROR:\nInvalid variable name.\n")
		return
	
	filename = asksaveasfilename(title = "Save As",filetypes = [("TI Bundle file",f"*.{bundle.get()}")],defaultextension = bundle.get(),initialfile = var_name.get())
	if filename == '': 
		return 
	
	rombundle = TIBundle(filename,ext=bundle.get())
	
	# add header file 
	rombundle.addData(rom.getHeader() + bytes(var_description.get(),encoding='utf-8') + b'\x00',var_name.get())
	# add PRG files 
	i = 0 
	for vardata in rom.getprg(): 
		rombundle.addData(vardata,var_name.get()+f"P{i:x}")
		i += 1 
	# add CHR files 
	i = 0 
	for vardata in rom.getchr(): 
		rombundle.addData(vardata,var_name.get()+f"C{i:x}")
		i += 1 
		
	rombundle.close()
	showinfo(message="Conversion complete!")
	return 
	
	
# UI code 
text_frame = ttk.Frame(frame,borderwidth = 1,relief = "groove",padding = (5,5))
text_frame.grid(column=0,row=0,rowspan = 3)
ttk.Label(text_frame,text = "Variable Name(MAX 6 Characters, Must Start With a Letter, Only alphanumeric)").grid(column=0,row=1)
ttk.Entry(text_frame,width = 10, textvariable = var_name).grid(column=0,row=2)
ttk.Label(text_frame,text = "ROM Description").grid(column=0,row=3)
ttk.Entry(text_frame,width = 40, textvariable = var_description).grid(column=0,row=4)
button = ttk.Button(text_frame, text='Browse', command=selectFile).grid(column=1,row=4)

# padding
ttk.Frame(frame,padding = (5,10)).grid(column=1,row=0)

radio_frame = ttk.Labelframe(frame,text = "Select bundle type:",borderwidth = 1,relief = "groove",padding = (5,5)) 
radio_frame.grid(column=2,row=0)
ttk.Radiobutton(radio_frame,text = "TI-84",variable = bundle,value = "b84").grid(column=0,row=1)
ttk.Radiobutton(radio_frame,text = "TI-83",variable = bundle,value = "b83").grid(column=0,row=2)


button = ttk.Button(frame, text='Convert', command=convertFile).grid(column=2,row=2)

Message(frame,textvariable = rominfo,width=80).grid(column=0,row=5,sticky = W)

root.mainloop() 
