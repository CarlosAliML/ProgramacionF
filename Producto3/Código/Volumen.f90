Program circle_area
  Implicit None
  Real *8 :: radius, high, diference, volume
  Real *8 :: PI = 4.0 * atan(1.0)
  Integer :: model_n = 1
  print *, 'Enter a radius:'
  print *, 'Enter high:'
  read (*,*) radius
  read (*,*) high
  diference = 3 * radius - high
  volume = 0.333 * PI * high * high * diference
  print *, 'Program number =' , model_n
  print *, 'radius =' , radius
  print *, 'High =' , high
  print *, 'Volume =' , volume
end Program circle_area 
 
