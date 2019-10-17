I created this repository in order to as a way to work with Reddit conversations using the Python Reddit API Wrapper ([PRAW](https://praw.readthedocs.io/en/latest/))

In the future, I'll turn this into a package called rraw or rwar. But first I need to learn more about the R's encapsulated object-oriented programming system ([R6](https://r6.r-lib.org/)). 

It also contains scripts for analyzing this data, and a couple of case studies.

```
├── README.md
├── redditAnalysis
│   ├── README.md
│   ├── net_reddit.R
│   ├── netproj_reddit.R
│   └── w2v_reddit.R
├── redditData
│   ├── Python
│   │   ├── authenticateAPI.py
│   │   ├── get_threads.py
│   │   ├── get_threads_by_keyword.py
│   │   └── search_keywords.py [temporary]
│   ├── R
│   │   ├── affiliationNetwork.R
│   │   ├── downloadURLs.R
│   │   └── getThread.R
│   └── README.md
└── secretInfo.R [hidden]
```

## Proof of Concept

___Getting the data___

Using the functions in `redditData/`, we download the latest 1000 threads from [/r/statistics](https://www.reddit.com/r/statistics/) using the functions in `downloadURLs.R`.

> This is a subreddit for discussion on all things dealing with statistical theory, software, and application. We welcome all researchers, students, professionals, and enthusiasts looking to be a part of an online statistics community.

We can download each individual thread using the functions in `getThread.R`. The output looks something like this:

insert OUTPUT data frame

We can then build an bipartite graph between users and subreddits using `affiliationNetwork.R`. The output looks something like this:

___Analyzing the data___

1. Word embeddings


2. Visualizing individual threads


3. Clustering the bipartite graph


___To do___

- Move the creation of word embeddings from the text2vec package to keras.

- Doing clustering on individual users too

- Improving the individual thread visualizations with JavaScript

