# Observ_models

This is part of project OBServ (www.observ.bees.science). 

This is an open repo to replicate [Garibaldi et al. 2011](https://onlinelibrary.wiley.com/doi/full/10.1111/j.1461-0248.2011.01669.x) and [2013](https://science.sciencemag.org/content/339/6127/1608) models. The idea is to fetch the data from a common repository where more data can be added and the models can be updated in real time. Maybe a Shiny app is in order. From the crop database a data paper can be produced. 

Two enhancements can be added. 
1) How to use different datasets in the same crop to capture crop level estimates and better present model outputs in the original units and with clear uncertainty measures.
2) Use a non linear function ([non-inflicted curve](https://www.jstor.org/stable/25661179?seq=1#metadata_info_tab_contents)) for estimating crop yield, as we know the function is not linear because of pollinator dependency changes, as well as it asimptotes. 

This will form the basis for implementing alternative models (traits, ML, mechanistic) and for cross-validation and transferability tests.

Notes:
- LU data can be extracted from k.lab
- Database can be isnpired in [BAAD](https://github.com/dfalster/baad) and in [Ethan White lab repos](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13104).
- Think on incorporating phenological aspects?
