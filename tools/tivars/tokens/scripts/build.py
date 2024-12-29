import json
import xml.etree.ElementTree as ET

from . import *


with open("8X.xml", encoding="UTF-8") as infile:
    root = ET.fromstring(src := infile.read())

    with open("built/8X.xml", "w+", encoding="UTF-8") as outfile:
        validate(root)
        outfile.write(src)

    with open("built/8X.json", "w+", encoding="UTF-8") as outfile:
        json.dump(to_json(root), outfile, indent=2, ensure_ascii=False)

with open(".github/workflows/tokenide.xml", encoding="UTF-8") as infile:
    sheet = TokenIDESheet.from_xml_string(infile.read())

    for model in "TI-82", "TI-83", "TI-83+", "TI-84+", "TI-84+CSE", "TI-84+CE":
        with open(f"built/tokenide/{model}.xml", "w+", encoding="UTF-8") as outfile:
            outfile.write(sheet.for_version(version=OsVersion(model, "latest")).to_xml_string())
