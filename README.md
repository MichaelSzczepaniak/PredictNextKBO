## Predicting the next word from a series of prior words using a Katz Backoff Trigram language model
This goal of this project was to build a data product which uses a Katz Backoff Trigram language model to the predict the next word from a series of prior words.  This is being implemented as a Shiny R web application which will be accessible from the following link:

<a href=https://michael-szczepaniak.shinyapps.io/predictnextword/>https://michael-szczepaniak.shinyapps.io/predictnextkbo/</a>

### Project Breakdown
The project is broken down in to four parts described below.  Each part contains a link to an page on rpubs which goes into further detail:
<ol style="list-style-type: decimal">
<li><a href=http://rpubs.com/mszczepaniak/predictkbo1preproc>Part 1 - Overview and Pre-Processing</a></li>
This main goal of this part was to convert the raw corpus data into a form which could be easily utilized by the next step in building n-gram tables and perform exploratory data analysis (EDA).
  <ul>
  <li>Background</li>
  <li>Project Objectives</li>
  <li>Acquiring, Partitioning, Preparing the Data
    <ul>
      <li>Sentence Parsing</li>
      <li>Non-ASCII Character Filtering</li>
      <li>Unicode Tag Conversions and Filtering</li>
      <li>URL Filtering</li>
      <li>Additional Filtering and EOS Tokenization</li>
    </ul>
  </li>
  </ul>
<li>Part 2 - N-grams and Exploratory Data Analysis</li>
The main goal of this part was to construct the n-gram tables which will be used by the language model and do some exploratory analysis on the cleaned up data.
  <ul>
  <li>Unigram Singleton Processing</li>
  <li>Unigram, Bigram, and Trigram Frequency Table Generation</li>
  </ul>
<li><a href=http://rpubs.com/mszczepaniak/predictkbo3model>Part 3 - Understanding and Implementing the Model</a></li>
The main goal of this part was to develop the conceptual framework and the code to implement the Katz Backoff Trigram algorithm as the model used to predict the next word.
  <ul>
  <li>Deriving the Model
    <ul>
      <li>Maximum Likelihood Estimate</li>
      <li>Markov Assumption</li>
      <li>Discounting</li>
      <li>Probabiltities of Observed N-grams</li>
      <li>Probabiltities of Unobserved N-grams</li>
    </ul>
  </li>
  <li>Defining and Implementing the KBO Trigram Alogrithm</li>
  </ul>
<li>Part 4 - Parameter Selection and Optimization</li>
In Part 3, generic values were used for the two parameters of the model: the bigram discount rate and trigram discount rate.  In this part, cross-validation is used to determine values for these discount rates to improve the accuracy of the model.  As time permits, additional performace optimizations will be performed.
<ul>
  <li>10-Fold Cross-Validation and Parameter Selection Strategy</li>
  <li>Improving Performace by Indexing N-gram Frequency Tables</li>
  </ul>
</ol>
