
readDataAxi :: Circuit (SimpleRD dom) (SimpleRD dom)
readDataAxi = Circuit (unbundle . fmap go . bundle)
  where
    go ::
      (ReadData.S2M_ReadData kr iw userType dataType, ReadData.M2S_ReadData) ->
      (ReadData.M2S_ReadData, ReadData.S2M_ReadData kr iw userType dataType)
    go = undefined

axiAdd1 :: (Num dataType) =>
      (ReadData.S2M_ReadData kr iw userType dataType, ReadData.M2S_ReadData) ->
      (ReadData.M2S_ReadData, ReadData.S2M_ReadData kr iw userType dataType)
axiAdd1 (s2mdata, m2sdata) = (m2sdata', s2mdata')
  where
    m2sdata' = m2sdata
    s2mdata' = s2mdata {
      ReadData._rdata = (ReadData._rdata s2mdata) + 1
    }
