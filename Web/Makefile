
2017/index.html: index.scrbl
	scribble --dest 2017/ index.scrbl

upload:
	raco s3-sync --web 2017/ s3://summer-school.racket-lang.org/2017
