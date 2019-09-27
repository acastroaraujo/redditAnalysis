I created this repository in order to as a way to work with Reddit conversations using the Python Reddit API Wrapper ([PRAW](https://praw.readthedocs.io/en/latest/))

In the future, I'll turn this into a package called rraw or rwar. But first I need to learn more about the R's encapsulated object-oriented programming system ([R6](https://r6.r-lib.org/)). 

It also contains scripts for analyzing this data, and a couple of case studies.

```
.
├── README.md
├── redditAnalysis
│   └── w2v_reddit.R
├── redditAnalysis.Rproj
└── redditData
    ├── Python
    │   ├── authenticateAPI.py
    │   ├── get_threads.py
    │   ├── helperFunctions.py
    │   └── search_keywords.py
    ├── R
    │   ├── affiliationNetwork.R
    │   ├── downloadURLs.R
    │   ├── getThread.R
    │   └── secretInfo.R [hidden]
    └── README.md
```