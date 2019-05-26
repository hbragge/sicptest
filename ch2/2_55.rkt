#lang sicp

(car ''acb)


 ;; (car ''something) is treated by the interpreter as: 
 ;; (car (quote (quote something))) 
 ;; The first occurrency of 'quote' quotes the next entity 
 ;; (quote something),which is actualy a list with two elements,so 
 ;; caring this list yileds 'quote.However,this is just a quoted 
 ;; symbol,not a procedure,typing quote in the interpreter prints: 
  