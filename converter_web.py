import convert

from pyscript import document, ffi, window

rom = None

async def on_file_selected(evt):
    [file] = evt.target.files
    print(f"{file.name}: {file.size} bytes")

    rom_data = (await file.bytes()).to_bytes()
    global rom
    try: 
        rom = convert.INes(rom_data)
        stem, _, _ = file.name.partition(".")
        document.getElementById("variableNameInput").value = stem[:6]
        document.getElementById("romDescriptionInput").value = stem
        print("ROM seems valid!")
    except Exception as e:
        print("ERROR:",str(e))
    

    

document.getElementById("fileSelect").addEventListener(
    "change", ffi.create_proxy(on_file_selected)
)

def on_submit(evt):
    evt.preventDefault()

    var_name = document.getElementById("variableNameInput").value
    bundle_kind = document.getElementById("bundleSelect").value
    try:
        bundle_data = convert.convertFile(
        rom,
        var_name,
        document.getElementById("romDescriptionInput").value,
        bundle_kind
        )
    except Exception as e: 
        print(str(e))
        return 
        
    print("Generated", len(bundle_data), "bytes of bundle")

    blob_url = window.URL.createObjectURL(window.Blob.new([ffi.to_js(bundle_data)]))
    blob_link = document.createElement("a")
    blob_link.download = f"{var_name}.{bundle_kind}"
    blob_link.href = blob_url

    document.body.appendChild(blob_link)
    blob_link.click()
    document.body.removeChild(blob_link)
    window.URL.revokeObjectURL(blob_url)
    print("Saved bundle")

document.getElementById("romForm").addEventListener(
    "submit", ffi.create_proxy(on_submit)
)

document.getElementById("convertButton").disabled = False
document.getElementById("fileSelect").disabled = False
document.getElementById("loadingSpinner").hidden = True