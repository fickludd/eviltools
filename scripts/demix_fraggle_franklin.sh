#!/bin/bash

IRTs=NatProt_empirical
IRT_MODE=simple-reg
WORKFLOW_BASE=demix_fraggle_franklin
DIA_FILE=/mnt/simon/fraggle_paper/swaths.txt
IRT_DEF=/mnt/common/RTpeptides/fraggle/iRT_${IRTs}.fraggle.tsv
DATABASE=/home/simon/demix_searches/fraggle_paper/mouse_organs/uniprot_refProteome_2015_10_MOUSE_RT.revCat.fasta
TRAMLER="java -jar /usr/local/eviltools/Tramler-0.8.2.free.jar"
FRAGGLE="java -jar /usr/local/eviltools/Fraggle-0.7.1.free.jar"
FRANKLIN="/home/johant/fraggle-paper/franklin"
FRANKLIN_MODE="all"
FEAT_APEX_RT_FLAG="false"
EXCLUDE_MODE="primary"

if [[ $# -eq 0 ]] ; then
        echo "provide ORGAN [iRT_DEF] [iRT_MODE] [USE_FEAT_APEX] [FRANKLIN_MODE]"
	echo "this script is to be executed in a directory containing"
	echo "	1 file named ORGAN.mzML.list containing one mzML base name per line"
	echo "  all mzML.gz files listed in the ORGAN.mzML.list"
	echo "  all demix search result .tsv files corresponsing to the lines in ORGAN.mzML.list"
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
        FEAT_APEX_RT_FLAG=$4
fi

if [[ $# -ge 5 ]] ; then
        FRANKLIN_MODE=$5
fi

WORKFLOW=${WORKFLOW_BASE}_${IRTs}_${IRT_MODE}_featRT-${FEAT_APEX_RT_FLAG}_frank-${FRANKLIN_MODE}

echo ""
echo "The script will store all results the directory $WORKFLOW"

ORGAN=$1
MS_FILES=$ORGAN.mzML.list
FINAL_NAME=${ORGAN}-${WORKFLOW}
ALL_IDS=${FINAL_NAME}.all
LOG_FILE="${FINAL_NAME}.log.txt"
TIMING_FILE="${FINAL_NAME}.time.tsv"
TRAML=${FINAL_NAME}.traml
DECOY_TRAML=${FINAL_NAME}.decoy.traml
FRANKLIN_OUT=${FINAL_NAME}.qvalues

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

T0=`timestamp`

rm temp1
for f in `cat ../$MS_FILES`; do
        BASE=`basename $f .mzML.gz`
	echo -n "../$BASE.tsv " >> temp1
done


ID_TSV_FILES=`cat temp1`
log "MS_FILES= $MS_FILES"
log "ID_TSV_FILES= $ID_TSV_FILES"

T1=`timestamp`

for f in `cat ../$MS_FILES`; do
        BASE=`basename $f .mzML.gz`
	DEMIX_ID=../$BASE.tsv
	exec "$FRAGGLE interpret --excludeMode=$EXCLUDE_MODE --irtMode=$IRT_MODE --useFeatureApexRT=$FEAT_APEX_RT_FLAG --psmFDR=0.01 --excludeProtPrefix=XXX --outName=${BASE}_$ORGAN --outDir=. ../$BASE.mzML.gz $DEMIX_ID $IRT_DEF"
done

exec "$FRAGGLE combine --outName=$FINAL_NAME *_$ORGAN.fragments.bin"

T2=`timestamp`
logTime "Fraggle" `expr $T2 - $T1`
#log "fraggle time taken: " `expr $T2 - $T1`

exec "$FRAGGLE export --mode=observation $FINAL_NAME.fragments.bin"
OBS_FILE=$FINAL_NAME.observations.tsv

exec "$FRANKLIN XXX $OBS_FILE"

T3=`timestamp`
logTime "Franklin" `expr $T3 - $T2`
#log "franklin time taken: " `expr $T3 - $T2`

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$TRAML --diaFile=$DIA_FILE stats 'qvalueFilter(qvalues=$FRANKLIN_OUT,mode=${FRANKLIN_MODE})' 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=y;b;m,diaRule)' stats"

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$DECOY_TRAML --diaFile=$DIA_FILE stats 'qvalueFilter(qvalues=$FRANKLIN_OUT,mode=${FRANKLIN_MODE})' stats 'decoy(decoy-factor=1)' stats 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=y;b;m,diaRule)' stats"


T4=`timestamp`
logTime "Tramler" `expr $T4 - $T3`
#log "tramler time taken: " `expr $T4 - $T3`

cd ..

