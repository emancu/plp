TestCase subclass: #Test04OperacionesConEscalares
 otro 
   aux := factores asOrderedCollection.
   
	aux do: [ :unit | nombres add: (unit nombre) ].