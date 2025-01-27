The *Arctic Holocene Biodiversity Map* project aims to reconstruct trends and stability of Arctic biodiversity during the Holocene. The project is led by researchers at the Scott Polar Research Institute, University of Cambridge, as part of a wider Horizon 2020 research consortium called <a href="https://charter-arctic.org" target="_blank">CHARTER</a>.

To achieve the goal of understanding trends and stability in Arctic biodiversity over the long-term, we approached the research in stages:

1. We first compiled a systematic map to gain an understanding of research that had been conducted into past biodiversity changes in the Arctic. *The systematic map progressing to completion but as yet unpublished*.

2. We expanded the database from (1) to include the underlying datasets, forming a database called the *Arctic Holcene Biodiversity Database* or AHBDB. The datasets were harmonised in terms of time and taxonomy to allow intercomparison across sources.

3. Lastly, here we apply the AHBDB to scaffold Arctic biodiversity indicators based on the framework of *Essential Biodiversity Variables*, to gain an understanding of trends and stability in biodiversity over time.

Here, we describe the common theory and methods used in (3). For specific methods on a per-indicator basis, please see the details on each indicator's page.

### Essential Biodiversity Variables

Within CHARTER, we have chosen to apply the framework of Essential Biodiversity Variables (EBVs - Pereira et al 2013) to frame the construction of biodiversity indicators for further uses (e.g. in inference models). This framework was chosen because biodiversity is a multidimensional concept that has yet to be summarised under a single metric, and the EBV framework is comprehensive and widely used to measure and describe such dimensions. We use the candidate set of EBVs specified by the Group on Earth Observations Biodiversity Observation Network (GEO BON) as the starting point for defining our long-term EBVs.

We have chosen to primarily focus on temporal dynamics and their variability in the multiple sites that the AHBD encompasses, rather than deriving inherently spatial indicators. This is because to derive spatially explicit indicators, proper quantification of the uncertainty surrounding the spatial dimension of each timeline would need to be estimated (e.g., the representative area of proxies from lake sediments, such as pollen and plant macrofossils) on a case-by-case basis. Each proxy has unique spatial footprints, which in addition can change i) through time as a function of e.g., dominant winds, productivity of the biological entities, amongst many other factors, or ii) within the proxy itself (e.g., as a function of different sizes of the same proxy material that may affect depositional processes). These uncertainties would then need to be propagated to candidate modelling approaches for interpolating or predicting variability in space within many time windows (e.g., 500-year bins). Although we recognise this as an important research direction, we have chosen to prioritise temporal over spatial dynamics in the first instance, as the spatial dynamics inherently contain far greater uncertainty and thus potentially less utility.

For temporal resolution, we calculated our indicators for 500-year time slices from 12,000 to 0 cal yr BP.

### Tools used for computation

The methods / code used to scaffold the Arctic biodiversity indicators from the Arctic Holocene Biodversity Database (AHBDB) are transparent and reside in the same GitHub repository as this website. Licence information is available in each repository.

Links: [Arctic Biodiveristy Indicators (data, website and scripts)](https://github.com/AndrewIOM/arctic-biodiversity-indicators/) / [Systematic map data coding software and database structure](https://github.com/AndrewIOM/biodiversity-graph-db/) / [The Arctic Holocene Biodiversity Database / systematic map dataset and scripts](https://github.com/AndrewIOM/holocene-arctic-biodiversity-map).


### Capturing Temporal and Taxonomic Uncertainties

As part of the compilation of the AHBDB, dates were harmonised to be cross-comparable, which included: harmonisation of timeline units; recalibration of radiocarbon dates to IntCal20; and construction of new uncertainty-based (probabilistic) age-depth models using OxCal 4.4 (Bronk Ramsey 2009). To quantify temporal uncertainty, we assume that dating methods that do not require age-depth models – wood ring cross-dating, historic events – are certain, versus uncertainty arising from the probabilistic age-depth models for timelines that require them.

The AHBDB captures the inference method used to translate between morphotypes / proxies and the ‘true’ underlying taxa. When using an atlas or key, inherent uncertainty may arise in the identity of the true taxa. Similarly, when the inference method is not stated in the literature but was implicit, there is higher uncertainty as to the plausible underlying taxa. We quantify this uncertainty and propagate it into our derived EBV indices.

### References
<hr/>
<p class="content is-small">Bronk Ramsey, C. (2009). Bayesian analysis of radiocarbon dates. Radiocarbon, 51(1), 337–360.</p>
<p class="content is-small">CAVM Team. 2024. Raster Circumpolar Arctic Vegetation Map. Scale 1:7,000,000. Conservation of Arctic Flora and Fauna, Akureyri, Iceland.</p>
<p class="content is-small">Pereira, H.M., Ferrier, S., Walters, M., Geller, G.N., Jongman, R.H.G., Scholes, R.J., et al. (2013). Essential biodiversity variables. Science, 339, 277–278.</p>