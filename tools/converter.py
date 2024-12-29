from tkinter import *
from tkinter import ttk
from tkinter.filedialog import *
from tkinter.messagebox import *

import zipfile
from tivars import *
from tivars.types import *

# import ines


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
		
	
	def addFile(self, name: str, arcname: str):
		self.zip.write(name,arcname) 
	
	def writeChecksum(self): 
		checksum = 0
		for info in self.zip.infolist(): 
			checksum += info.CRC
		checksum &= 0xFFFFFFFF
		self.zip.writestr("_CHECKSUM",f"{checksum:x}\r\n")
	
	def close(self): 
		self.zip.close()




supported_mappers = (0) 

root = Tk() 
root.title("NES to Appvar Utility") 
frame = ttk.Frame(root,padding = (5,10))
frame.grid(column=0,row=0)

filename = ''
rom = None 

var_name = StringVar()
var_description = StringVar()
bundle = StringVar()
rominfo = StringVar()

bundle.set("b84")

def selectFile(): 
	filename = askopenfilename(title = "Please select a file",filetypes = [("NES ROM files","*.nes")])
	var_description.set(filename.rsplit("/")[-1].rsplit("\\")[-1].removesuffix(".nes"))
	rominfo.set("Line 0\nLine1\nLine2")
	return 

def convertFile():
	showerror(message = "File cannot be found")
	return 
	
text_frame = ttk.Frame(frame,borderwidth = 1,relief = "groove",padding = (5,5))
text_frame.grid(column=0,row=0,rowspan = 3)
ttk.Label(text_frame,text = "Variable Name(MAX 6 Characters, Must Start With a Letter, Only alphanumeric)").grid(column=0,row=1)
ttk.Entry(text_frame,width = 6, textvariable = var_name).grid(column=0,row=2)
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

Message(frame,textvariable = rominfo).grid(column=0,row=5,sticky = W)

root.mainloop() 
