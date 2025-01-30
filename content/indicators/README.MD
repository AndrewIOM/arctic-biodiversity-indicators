Arctic Biodiversity Indicators for the Holocene
===

This dataset is a derivative database from the Arctic Holocene Biodiversity Database, as as such is licensed under the Open Database License (see LICENCE).

Here, each folder corresponds to an *Essential Biodiversity Variable* class, within which are individual indicators. All indicators follow a standard tab-delimited format.

Temporal indicators (on a per-timeline basis) are included in a .tsv file, whereas the spatial aggregate of the indicator (on the basis of phytogeographic realm * bioclimate subzone) are in a file of the same name but with _spatial.tsv rather than .tsv at the end. In the spatial files, the location_id corresponds to a polygon ID in the geojson file at `src/web/wwwroot/content/geo/phyto-subzones-simplified.json`.