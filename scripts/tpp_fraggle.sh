#!/bin/bash

IRTs=NatProt_empirical
IRT_MODE=simple-reg
WORKFLOW_BASE=tpp_fraggle
DIA_FILE=/mnt/simon/fraggle_paper/swaths.txt
IRT_DEF=/mnt/common/RTpeptides/fraggle/iRT_${IRTs}.fraggle.tsv
DATABASE=/mnt/simon/database/uniprot_refProteome_2015_10_MOUSE_RT.DECOY.fasta
TRAMLER="java -jar /usr/local/eviltools/Tramler-0.8.2.free.jar"
FRAGGLE="java -jar /usr/local/eviltools/Fraggle-0.7.1.free.jar"
FRAG_TOL=10ppm
ALLOWED_FRAGMENTS="y;b"

if [[ $# -eq 0 ]] ; then
	echo "provide ORGAN [iRT_DEF] [iRT_MODE] [FRAG_MATCH_TOL] [FRAGMENT_TYPES] [DATABASE]"
	echo "this script is to be executed in a directory containing"
        echo "  1 file named ORGAN.mzML.list containing one mzML base name per line"
        echo "  all mzML.gz files listed in the ORGAN.mzML.list"
	echo "  1 iProphet.pep.xml corresponding to the TPP search result of the files in ORGAN.mzML.list"
        echo ""
        echo "it further assumed the existence of"
        echo "  $DIA_FILE"
        echo "  $IRT_DEF"
        echo "  $DATABASE"
	exit 1
fi

if [[ $# -ge 2 ]] ; then
        IRTs=$2
        IRT_DEF=/mnt/common/RTpeptides/fraggle/iRT_${IRTs}.fraggle.tsv
fi

if [[ $# -ge 3 ]] ; then
	IRT_MODE=$3
fi

if [[ $# -ge 4 ]] ; then
	FRAG_TOL=$4
fi

if [[ $# -ge 5 ]] ; then
	ALLOWED_FRAGMENTS=$5
fi

if [[ $# -ge 6 ]] ; then
	DATABASE=$6
fi

SAFE_FRAGMENTS=`echo $ALLOWED_FRAGMENTS | sed 's/;/_/g'`
WORKFLOW=${WORKFLOW_BASE}_${IRTs}_${IRT_MODE}_${FRAG_TOL}_${SAFE_FRAGMENTS}

echo ""
echo "The script will store all results the directory $WORKFLOW"

ORGAN=$1
IPROPH=iProphet_$ORGAN
IPROPH_XML=$IPROPH.pep.xml
MS_FILES=$ORGAN.mzML.list
FINAL_NAME=${ORGAN}-${WORKFLOW}
MAYU_PSM=${FINAL_NAME}_psm_protFDR0.01_t_1.07.csv
LOG_FILE="${FINAL_NAME}.log.txt"
TIMING_FILE="${FINAL_NAME}.time.tsv"
TRAML=${FINAL_NAME}.traml

timestamp() {
  date +%s
}

log() {
  echo "$@"
  echo "$@" >> $LOG_FILE
}

logTime() {
  log "$1 time taken: $2"
  echo "$ORGAN\t$WORKFLOW\t$1\t$2" >> $TIMING_FILE
}

exec() {
  log "$@"
  eval "$@"
}

mkdir -p $WORKFLOW
cd $WORKFLOW
#rm $TIMING_FILE
ln -s ../$IPROPH_XML $IPROPH_XML

T0=`timestamp`

exec "Mayu.pl -A $IPROPH_XML -C $DATABASE -E DECOY -G 0.01 -H 51 -I 2 -v -P protFDR=0.01:t -M $FINAL_NAME"

#LAST_MAYU_OUTPUT=`ls -c *psm_protFDR*_1.07.csv | head -n 1`
log "used mayu output file '$MAYU_PSM'"

MAYU_CUTOFF=`tail -n +2 $MAYU_PSM | awk -F ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' | awk -F ',' '{print $5}'`
log "MAYU_CUTOFF = tail -n +2 $MAYU_PSM | awk -F" ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' "| awk -F" ',' '{print $5}'
log "MAYU_CUTOFF = $MAYU_CUTOFF"

T1=`timestamp`
logTime "Mayu" `expr $T1 - $T0`
#log "mayu time taken: " `expr $T1 - $T0`

exec "pepxml2csv $IPROPH_XML"

for f in `cat ../$MS_FILES`; do
	BASE=`basename $f .mzML.gz`
	echo $f $BASE
	exec "$FRAGGLE interpret --peptideProb=$MAYU_CUTOFF --excludeProtPrefix=DECOY --fragMatchTol=$FRAG_TOL --irtMode=$IRT_MODE --irtR2=0.8 --outName=${BASE}_$ORGAN --outDir=. ../$BASE.mzML.gz $IPROPH.pep.csv $IRT_DEF"
done

exec "$FRAGGLE combine --outName=$FINAL_NAME *_$ORGAN.fragments.bin"

T2=`timestamp`
logTime "Fraggle" `expr $T2 - $T1`
#log "fraggle time taken: " `expr $T2 - $T1`

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$TRAML --diaFile=$DIA_FILE 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=$ALLOWED_FRAGMENTS,diaRule)' stats" 

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$DECOY_TRAML --diaFile=$DIA_FILE 'decoy(decoy-factor=1)' stats 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=$ALLOWED_FRAGMENTS,diaRule)' stats"

T3=`timestamp`
logTime "Tramler" `expr $T3 - $T2`
#log "tramler time taken: " `expr $T3 - $T2`

cd ..
