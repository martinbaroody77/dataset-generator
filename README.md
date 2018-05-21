Generates random train and test datasets with some underlying pattern. Good for testing machine learning algorithms on.

This app, created in R and Shiny, allows users to tweak the number of variables, complexity of the pattern, and amount of deviation from the pattern of the datasets, as well as the number of rows of data for both the training and test datasets. Once the user customizes these attributes of the dataset, they can click on "generate datasets" and then download them with the corresponding buttons. The datasets are in CSV format. 

My algorithm makes it such that there is an underlying pattern in the data, and users can then train their machine learning models on the training dataset and test it on the test dataset. The "complexity" attribute refers to how complicated the underlying pattern is. If complexity is low, then the pattern will simply be a linear equation of the variables with random coefficients. The higher complexities allow for higher exponents. The "deviation" parameter refers to how much error is allowed. "No deviation" means there's no error at all.

Right now the dataset generator only supports regression problems, I might allow for more types of data in the future.
