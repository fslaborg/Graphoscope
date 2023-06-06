(*** hide ***)

(*** condition: prepare ***)
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Graphoscope
 
[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/plotly.net/gh-pages?urlpath=/tree/home/jovyan/{{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

A pragmatic approach to network science.

Here is how to use a function in the docs from the DLL built from the project:
*)

open Graphoscope

GraphPlaceholder.hello "world"
(*** include-it ***)

(**
# Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][docs] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under the OSI-approved MIT license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [docs]: https://github.com/fslaborg/Graphoscope/tree/main/docs
  [gh]: https://github.com/fslaborg/Graphoscope
  [issues]: https://github.com/fslaborg/Graphoscope/issues
  [readme]: https://github.com/fslaborg/Graphoscope/blob/main/README.md
  [license]: https://github.com/fslaborg/Graphoscope/blob/main/LICENSE
*)
