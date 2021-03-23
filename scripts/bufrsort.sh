#!/bin/sh
bdate=2020123112
bufrdir=/scratch1/NCEPDEV/stmp4/Kristen.Bathmann/iasibufr
ins=M01 
#M01-IASI-B
#M02-IASI-A
#M03-IASI-C
if [ $ins == M01 ] ; then
  ipr=mb
fi
if [ $ins == M02 ] ; then
  ipr=ma
fi
if [ $ins == M03 ] ; then
  ipr=mc
fi
#prfx is 25
prfx=${ins}-IASI-IASPCS1C-NA-1.0-
cd $bufrdir
[ ! -d ${bdate} ] && mkdir ${bdate}
yr=${bdate:0:4}
mon=${bdate:4:2}
day=${bdate:6:2}
hr=${bdate:8:2}
f0=00
f6=06
f12=12
f18=18
m1=01
one=1
if [ $hr == $f0 ] ; then
  fs=20
  fe=21
  ds=$((day - one))
fi
if [ $hr == $f6 ] ; then
  fs=02
  fe=03
  ds=$day
fi
if [ $hr == $f12 ] ; then
  fs=08
  fe=09
  ds=$day
fi
if [ $hr == $f18 ] ; then
  fs=14
  fe=15
  ds=$day
fi
ys=$yr
ms=$mon
jan=01
feb=02
mar=03
apr=04
may=05
jun=06
jul=07
aug=08
sep=09
oct=10
nov=11
dec=12
if [ $day == $m1 ] ; then
  if [ $hr = $f00 ] ; then
    ms=$((ms - one))
    if [ $ms == $f00 ] ; then
        ms =12 
        ys=$((ys - one))
    fi
    if [ $ms == $jan ] || [ $ms == $mar ] || [ $ms == $may ]  || [ $ms == $jul ] || [ $ms == $aug ] || [$ms == $oct ] || [ $ms == $dec ] ; then 
      ds=31
    fi
    if [ $ms == $apr ] || [ $ms == $jun ] || [ $ms == $sep ] || [ $ms == $nov ] ; then
      ds=30
    fi
    if [ $ms == $feb ] ; then
      ds =28
      if [ $yr == 2020 ] ; then 
        ds =29
      fi
    fi 
  fi
fi

for fil in `ls $bufrdir` ; do
  inche=${fil:0:3}
  if [ $inche == $ins ] ; then
     yf=${fil:25:4}
     monf=${fil:29:2}
     dayf=${fil:31:2}
     hrf=${fil:33:2}
     if [ $yf == $ys ] ; then
       if [ $ms == $monf ] ; then
         if [ $ds == $dayf ] ; then
           if [ $hrf -ge $fs ] ; then
             if [ $hrf -le $fe ] ; then

               filex=$fil
             fi
           fi
         fi
       fi
     fi

  fi
done
nf=1
for fil in `ls $bufrdir` ; do
  inche=${fil:0:3}
  if [ $inche == $ins ] ; then
    if [ $fil == $filex ] ; then
      if [ $nf == 1 ] ; then
#        cp $fil $bdate/iasibufr${ipr}${nf}.bfr
        echo $fil $nf
        nf=$(( nf + one))
      fi
    fi
    if [ $nf -le 7 ] ; then
      if [ $nf -ge 2 ] ; then
#        cp $fil $bdate/iasibufr${ipr}${nf}.bfr
        echo iasibufr${ipr}${nf}.bfr
        nf=$(( nf + one ))
      fi
    fi
  fi
done
