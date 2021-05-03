splineOperator = function(df,spar, allKnots = TRUE, nKnots){
  smm = smooth.spline(x = df$value.x, y = df$value.y, spar = spar, all.knots = allKnots, nknots = nKnots)
  xFit = df$value.x
  yFit = fitted(smm)
  yRes = resid(smm)
  res = data.frame(rowSeq = df$rowSeq,colSeq = df$colSeq,  xFit = xFit, yFit = yFit, yRes = yRes, yData = df$value.y)
}
