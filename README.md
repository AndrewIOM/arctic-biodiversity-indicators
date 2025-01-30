# Arctic Biodiversity Indicators for the Holocene
Indicators of Arctic biodiversity change over the last 12,000 years (the Holocene).

Within the CHARTER Horizon 2020 project, we aimed to reconstruct trends and stability of Arctic biodiversity during the Holocene. To achieve the goal of understanding trends and stability in Arctic biodiversity over the long-term, we developed tools to scaffold Arctic biodiversity indicators based on the framework of *Essential Biodiversity Variables* (Pereira et al 2013). We applied the tools to the [Arctic Holocene Biodiversity Database](https://github.com/AndrewIOM/holocene-arctic-biodiversity-map) dataset, to gain an understanding of trends and stability in biodiversity over time.

We created a suite of key indicators and a web-based tool to visualise and access them. To achieve this, we: (a) identified candidate indicator variables; (b) applied taxonomic and temporal harmonisation within the AHBDB; (c) implemented code to scaffold the candidate indicator variables from the latest AHBDB at any given time; and (d) implemented a web app to display visualisations of the key indicators including slicing, aggregating and filtering by taxonomic, spatial and temporal axes.

This repository contains:

* A console application in `src/essential-bio-variables` to (a) harmonise dates between sources and (b) scaffold key biodiversity indicators.

* A resultant dataset (licenced under the Open Database License) in `data-derived`.

* Third party datasets required to to make the key biodiversity indicators in `data-third-party`.

* An F# Bolero webassembly application in `src/web` that may be used to visualise the datasets.

#### Running locally

The only requirement for the web application is to have .NET 9 installed for compile. In the `src` folder, run a `dotnet paket install` to set up dependencies. Then, in the `web` folder, `dotnet build` and `dotnet run` should suffice.

To run the essential biodiversity scaffolding, some additional requirements must be met. You must have R installed and the `oxcAAR` package. GDAL is used but does not require a system install; however, you should modify the Nuget package to be appropriate to your platform in `paket.references` and `paket.dependencies` (e.g. from macos to windows if on windows) and run a `dotnet paket install` to restore. A local copy of the Arctic Holocene Biodiversity Database is also required; the repository should be placed in the same parent folder as this repository is.

#### References

* Pereira, H.M., Ferrier, S., Walters, M., Geller, G.N., Jongman, R.H.G., Scholes, R.J., et al. (2013). Essential biodiversity variables. Science, 339, 277–278.