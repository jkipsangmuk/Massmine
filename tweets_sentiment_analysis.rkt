#lang racket
;;; data-science to process the text, and plot to visualize the results
;;;The math provides functions and data structures useful for working with numbers and collections of numbers
(require data-science-master)
(require plot)
(require math)
;Reads all characters from in and returns them as a string. The input port is closed unless close? is #f.
;opens the file specified by path for input. The mode-flag argument specifies how the file’s bytes are translated on input:
(define tweet-read (port->string(open-input-file "UgandaDataQuery.csv")))

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
(define clean-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase tweet-read))))

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens clean-text #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 1)

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))