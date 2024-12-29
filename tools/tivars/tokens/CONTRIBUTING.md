# Contributing

If you'd like to contribute to the token sheets, we'd greatly appreciate it! Listed below are the main ways you can help us out and what each entails.

## Verifying Translations

Does a translation have a typo? Is there a variant name that should be added? We appreciate any and all small fixes to the sheets, which you can help us spot simply by using them.

## Adding New Translations

One important goal of this project is to maintain token translations for every language supported by TI, and there are a lot! Luckily, each language's token and font translations are bundled in a single .8ek file, which can be parsed using [this tool](https://github.com/TI-Toolkit/token_translation_extractor). We _strongly_ recommend using it or a similar tool instead of trying to manually transcribe information from a calculator.

Further tooling and documentation to streamline adding .8ek information to the sheets is in the works.

## Adding New Scripts

If you've written a script involving the token sheets that has general utility, feel free to make a PR to add it to the `scripts` package. Be sure to document your code and add relevant imports to `__init__.py` so it may be incorporated into the package.