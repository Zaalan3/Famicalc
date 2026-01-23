import enum
import io
import zipfile

from tivars import *
from tivars.types import *

def batched(iterable, size):
    """Yield successive n-sized chunks from an iterable.
    
    Nearly the same as itertools.batched, but slices the input iterable rather
    than yielding a tuple of elements.
    """
    for i in range(0, len(iterable), size):
        yield iterable[i:i + size]


class InvalidROM(Exception):
    pass


class INes: 
    def __init__(self, data: bytes):
        self.header = data[0:16]
        self.data = data[16:]
        self._validate()
        
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
    
    def _validate(self):
        supported_mappers = [0,1,2,3,4,7]
        # a number of conditions to see if the rom can be transferred to calc.
        
        if not(self.header[0:4] == b"NES\x1a"): 
            raise InvalidROM("Invalid ROM header")
        if self.prgsize() < 0 or self.prgsize() > 32:
            raise InvalidROM(f"ROM too large: {self.prgsize()} 16k pages")
        elif self.chrsize() < 0 or self.chrsize() > 64:
            raise InvalidROM(f"ROM too large: {self.chrsize} 8k pages")
        elif not(self.mapper() in supported_mappers):
            raise InvalidROM(f"Unsupported mapper: {self.mapper()}")
        elif len(self.data) != 16384*self.prgsize() + 8192*self.chrsize():
            raise InvalidROM(f"Invalid data size: {len(self.data)} bytes in {self.prgsize()} 16k pages and {self.chrsize()} 8k pages")
    
    def getHeader(self): 
        return b"FAMICALC" + bytes([self.mapper(),self.mirroringInt(),self.prgsize(),self.chrsize()])
    
    def getprg(self): 
        prg = self.data[0:16384*self.prgsize()]
        return batched(prg,16384*3)
    
    def getchr(self): 
        chr = self.data[16384*self.prgsize():]
        return batched(chr,8192*7)
    

class TIBundle:

    class Kind(enum.StrEnum):
        B83 = "b83"
        B84 = "b84"

    _METADATA = {
        Kind.B83: (
            "bundle_identifier:TI Bundle\n"
            "bundle_format_version:1\n"
            "bundle_target_device:83CE\n"
            "bundle_target_type:CUSTOM\n"
            "bundle_comments:N/A\n"
        ),
        Kind.B84: (
            "bundle_identifier:TI Bundle\n"
            "bundle_format_version:1\n"
            "bundle_target_device:84CE\n"
            "bundle_target_type:CUSTOM\n"
            "bundle_comments:N/A\n"
        ),
    }

    def __init__(self, ext: Kind):
        self._data = io.BytesIO()
        self.zip = zipfile.ZipFile(self._data, "w", allowZip64=False)
        self.ext = ext
        self.zip.writestr("METADATA", self._METADATA[ext])
    
    @property
    def zipData(self) -> bytes:
        return self._data.getvalue()

    def addData(self, data: bytes, arcname: str):
        # make file to add to zip
        appvar = TIAppVar(name=arcname, data=data)
        self.zip.writestr(arcname + ".8xv", appvar.export().bytes())

    def writeChecksum(self):
        checksum = 0
        for info in self.zip.infolist():
            checksum += info.CRC
        checksum &= 0xFFFFFFFF
        self.zip.writestr("_CHECKSUM", f"{checksum:x}\r\n")

    def close(self):
        self.writeChecksum()
        self.zip.close()


def convertFile(rom: INes, var_name: str, var_description: str, bundle: TIBundle.Kind):
    if not var_name:
        raise ValueError("Variable name must not be empty")
    elif len(var_name) > 6:
        raise ValueError(f"Variable name must not be longer than 6 characters (was {len(var_name)})")
    elif not var_name.isalnum():
        raise ValueError("Variable name must only contain letters and numbers")
    elif not var_name[0].isalpha():
        raise ValueError("First character of variable name must be a letter")

    rombundle = TIBundle(bundle)

    # add header file
    rombundle.addData(
        rom.getHeader() + bytes(var_description, encoding="ascii") + b"\x00",
        var_name.upper(),
    )
    # add PRG files
    i = 0
    for vardata in rom.getprg():
        rombundle.addData(vardata, var_name.upper() + f"P{i:X}")
        i += 1
    # add CHR files
    i = 0
    for vardata in rom.getchr():
        rombundle.addData(vardata, var_name.upper() + f"C{i:X}")
        i += 1

    rombundle.close()
    return rombundle.zipData
