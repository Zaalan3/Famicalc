import convert

from pyscript import document, ffi, window


class WebConverter:
    def __init__(self):
        self.rom = None

    async def on_file_selected(self, evt):
        self.rom = None
        [file] = evt.target.files
        print(f"{file.name}: {file.size} bytes")

        rom_data = (await file.arrayBuffer()).to_bytes()
        try: 
            self.rom = convert.INes(rom_data)
            stem, _, _ = file.name.partition(".")
            document.getElementById("variableNameInput").value = stem[:6]
            document.getElementById("romDescriptionInput").value = stem
            print("ROM seems valid!")
        except Exception as e:
            print("ERROR:",str(e))

    @staticmethod
    def _download_data(data: bytes, filename: str):
        blob_url = window.URL.createObjectURL(window.Blob.new([ffi.to_js(data)]))
        blob_link = document.createElement("a")
        blob_link.download = filename
        blob_link.href = blob_url

        document.body.appendChild(blob_link)
        blob_link.click()
        document.body.removeChild(blob_link)
        window.URL.revokeObjectURL(blob_url)

    def on_submit(self, evt):
        evt.preventDefault()

        var_name = document.getElementById("variableNameInput").value
        bundle_kind = document.getElementById("bundleSelect").value
        try:
            bundle_data = convert.convertFile(
                self.rom,
                var_name,
                document.getElementById("romDescriptionInput").value,
                bundle_kind
            )
        except Exception as e: 
            print(str(e))
            return 
            
        print("Generated", len(bundle_data), "bytes of bundle")

        self._download_data(bundle_data, f"{var_name}.{bundle_kind}")
        print("Saved bundle")


converter = WebConverter()

document.getElementById("fileSelect").addEventListener(
    "change", ffi.create_proxy(converter.on_file_selected)
)

document.getElementById("romForm").addEventListener(
    "submit", ffi.create_proxy(converter.on_submit)
)

# Enable UI once we're all set up to handle input
document.getElementById("convertButton").disabled = False
document.getElementById("fileSelect").disabled = False
document.getElementById("loadingSpinner").hidden = True