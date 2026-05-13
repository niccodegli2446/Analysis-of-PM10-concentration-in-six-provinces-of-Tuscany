# Analysis-of-PM10-concentration-in-six-provinces-of-Tuscany
The thesis addresses the topic of air quality in Tuscany through the analysis of PM10 concentration in six provinces (Arezzo, Livorno, Prato, Florence, Pisa, and Lucca) in the period 2020–2023. The objective is twofold: to evaluate the links between particulate concentration and meteorological conditions and to explore eventual phenomena of spatial and temporal interdependence among the provinces.

After a theoretical introduction on atmospheric pollutants and in particular on PM10, the data used are described, collected by ARPAT for pollution and by the Regional Hydrological Service for the weather. Particular attention was placed on the management of missing data, resolved through linear interpolations for meteorological variables and ARIMA models with external regressors for the PM10 series.

The descriptive analysis shows how PM10 levels tend to increase in the winter months and how the inland provinces (especially Lucca, Arezzo, and Prato) present higher concentrations compared to Livorno, which benefits from the mitigating effect of the wind. Furthermore, the analysis shows how meteorological factors play a fundamental role in the variation of PM10 concentration in the air.

Through SURE (Seemingly Unrelated Regression Equations) models, a significant interdependence between provinces is highlighted, a sign that pollution episodes are not isolated but influenced by common territorial dynamics. The results also confirm the determinant role of meteorological conditions, defining a meteorological combination that favors the increase of PM10 concentration in the air: low temperatures, absence of wind, high humidity, and scarce precipitation.

The conclusions therefore underline how specific meteorological combinations represent the most favorable conditions for the accumulation of PM10 and how the interaction between provinces makes an integrated approach to environmental policies necessary. The research highlights, finally, possible future developments linked to more complex predictive models and the integration with other sources of pollution.

### Repository Structure
- `dati_PM10` and `dati_meteo_s.toscana`: Datasets used for the analysis (PM10 concentrations and meteorological data).
- `funzioni utili`: Custom functions developed for ARIMA and SURE models.
- `data_load.R`, `fill_missing.R`, `descrittive PM10-meteo.R` and `models.R`: Full analysis workflow, ranging from data pre-processing to SURE model estimation.
