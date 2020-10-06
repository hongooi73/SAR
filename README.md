# SAR: Smart Adaptive Recommendations

[SAR](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/sar.md) is a practical, rating-free collaborative filtering algorithm for recommendations. It produces explainable results, and is usable on a wide range of problems.

This package provides the following:

- An R interface to the Azure [Product Recommendations](https://github.com/Microsoft/Product-Recommendations) service, a cloud implementation of SAR. It includes the ability to deploy the backend via the [AzureRMR](https://github.com/AzureR/AzureRMR) package, as well as a client frontend.

- A standalone R implementation of SAR, for ease of experimentation and familiarisation. The core algorithm is written in C++ and makes use of multithreading and sparse matrices for speed and efficiency.

## More information

[A detailed description of SAR](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/sar.md)

Other SAR implementations:

- Python: [Microsoft/Recommenders](https://github.com/Microsoft/Recommenders)
- Spark: [Azure/mmlspark](https://github.com/Azure/mmlspark)

