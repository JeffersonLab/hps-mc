C     Diagram 1
      DATA MAPCONFIG(1)/1/
      DATA (IFOREST(I,-1,1),I=1,2)/1,4/
      DATA TPRID(-1,1)/11/
      DATA (SPROP(I,-1,1),I=1,1)/0/
      DATA (IFOREST(I,-2,1),I=1,2)/-1,3/
      DATA TPRID(-2,1)/22/
      DATA (SPROP(I,-2,1),I=1,1)/0/
      DATA (IFOREST(I,-3,1),I=1,2)/-2,5/
C     Diagram 2
      DATA MAPCONFIG(2)/2/
      DATA (IFOREST(I,-1,2),I=1,2)/4,3/
      DATA (SPROP(I,-1,2),I=1,1)/11/
      DATA TPRID(-1,2)/0/
      DATA (IFOREST(I,-2,2),I=1,2)/1,-1/
      DATA TPRID(-2,2)/22/
      DATA (SPROP(I,-2,2),I=1,1)/0/
      DATA (IFOREST(I,-3,2),I=1,2)/-2,5/
C     Number of configs
      DATA MAPCONFIG(0)/2/
