## functions

# Rotate 90 function
rotateMatrix90 = function(mat)
{
  matB = matrix (c(
    0,0,1,
    0,1,0,
    1,0,0
  ), nrow=3, byrow=T);
  
  result = t(matB%*%mat)
  result;
}

# Rotate 180 function
rotateMatrix180 = function(mat)
{
  matB = matrix (c(
    0,0,1,
    0,1,0,
    1,0,0
  ), nrow=3, byrow=T);
  
  turn1 = t(matB%*%mat);
  t(matB%*%turn1)
}

# Rotate 270 function
rotateMatrix270 = function(mat)
{
  matB = matrix (c(
    0,0,1,
    0,1,0,
    1,0,0
  ), nrow=3, byrow=T);
  
  turn1 = t(matB%*%mat);
  turn2 = t(matB%*%turn1);
  t(matB%*%turn2)
}
