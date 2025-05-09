# 🧠 Naïve Bayes and k-NN Classification on Pima Indians Diabetes Dataset

This project demonstrates how to perform classification using the **Naïve Bayes** and **k-Nearest Neighbors (k-NN)** algorithms on the Pima Indians Diabetes dataset in **R**. The dataset is included in the `MASS` package and is commonly used for binary classification tasks (diabetic: Yes/No).

## 📊 Dataset Information

The dataset includes medical data for female Pima Indian patients, such as:

- `glu`: Plasma glucose concentration
- `ped`: Diabetes pedigree function
- `type`: Outcome variable (Yes/No for diabetes)

The dataset is divided into:
- `Pima.tr`: Training set
- `Pima.te`: Testing set

## 📦 Packages Used

- [`MASS`](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/00Index.html)
- [`e1071`](https://cran.r-project.org/web/packages/e1071/index.html) (for Naïve Bayes)
- [`class`](https://stat.ethz.ch/R-manual/R-devel/library/class/html/00Index.html) (for k-NN)

## 🚀 Installation

1. Clone the repo:

   ```bash
   git clone https://github.com/your-username/naivebayes-knn-diabetes.git
   cd naivebayes-knn-diabetes
