# An optional custom script to run before Hugo builds your site.
# You can delete it if you do not need it.
allpubs_snapshot <- readRDS("R/allpubs.RDS")
allpubs_newsnapshot <- fileSnapshot(path = "content/allpubs.bib", md5sum = TRUE)
changed <- changedFiles(allpubs_snapshot, allpubs_newsnapshot)
if(!is.null(changed$changes)){
  blogdown:::build_rmds("content/papers.Rmd")
  allpubs_snapshot <- fileSnapshot(path = "content/allpubs.bib", md5sum = TRUE)
  saveRDS(allpubs_snapshot, "R/allpubs.RDS")
}

tutorials_snapshot <- readRDS("R/tutorials.RDS")
tutorials_newsnapshot <- fileSnapshot(path = "content/tutorials.bib", md5sum = TRUE)
changed <- changedFiles(tutorials_snapshot, tutorials_newsnapshot)
if(!is.null(changed$changes)){
  blogdown:::build_rmds("content/books-tutorials.Rmd")
  tutorials_snapshot <- fileSnapshot(path = "content/tutorials.bib", md5sum = TRUE)
  saveRDS(tutorials_snapshot, "R/tutorials.RDS")
}
