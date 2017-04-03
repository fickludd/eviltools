#!/bin/bash

IRTs=NatProt_empirical
IRT_MODE=simple-reg
WORKFLOW_BASE=demix_fraggle
DIA_FILE=/mnt/simon/fraggle_paper/swaths.txt
IRT_DEF=/mnt/common/RTpeptides/fraggle/iRT_${IRTs}.fraggle.tsv
DATABASE=/home/simon/demix_searches/fraggle_paper/mouse_organs/uniprot_refProteome_2015_10_MOUSE_RT.revCat.fasta
TRAMLER="java -jar /usr/local/eviltools/Tramler-0.8.2.free.jar"
FRAGGLE="java -jar /usr/local/eviltools/Fraggle-0.7.1.free.jar"
FEAT_APEX_RT_FLAG="false"
EXCLUDE_MODE="full"

if [[ $# -eq 0 ]] ; then
        echo "provide ORGAN [iRT_DEF] [iRT_MODE] [USE_FEAT_APEX]"
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

WORKFLOW=${WORKFLOW_BASE}_${IRTs}_${IRT_MODE}_featRT-${FEAT_APEX_RT_FLAG}

echo ""
echo "The script will store all results the directory $WORKFLOW"

ORGAN=$1
MS_FILES=$ORGAN.mzML.list
FINAL_NAME=${ORGAN}-${WORKFLOW}
MAYU_PSM=${FINAL_NAME}_psm_protFDR0.01_t_1.07.csv
ALL_IDS=${FINAL_NAME}.all
LOG_FILE="${FINAL_NAME}.log.txt"
TIMING_FILE="${FINAL_NAME}.time.tsv"
TRAML=${FINAL_NAME}.traml
DECOY_TRAML=${FINAL_NAME}.decoy.traml

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

TEMP=$FINAL_NAME.temp
rm $TEMP
for f in `cat ../$MS_FILES`; do
        BASE=`basename $f .mzML.gz`
	echo -n "../$BASE.tsv " >> $TEMP
done


ID_TSV_FILES=`cat $TEMP`
log "MS_FILES= $MS_FILES"
log "ID_TSV_FILES= $ID_TSV_FILES"

exec "merge-csv -o $ALL_IDS.tsv $ID_TSV_FILES"

exec "msgf-tsv2mayu --stupidFirst XXX $ALL_IDS.tsv"

exec "Mayu.pl -B $ALL_IDS.mayu.csv -C $DATABASE -E XXX -G 0.01 -H 51 -I 2 -v -P protFDR=0.01:t -M $FINAL_NAME"

log "used mayu output file '$MAYU_PSM'"

MAYU_CUTOFF=`tail -n +2 $MAYU_PSM | awk -F ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' | awk -F ',' '{print $5}'`
log "MAYU_CUTOFF = tail -n +2 $MAYU_PSM | awk -F" ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' "| awk -F" ',' '{print $5}'
log "MAYU_CUTOFF = $MAYU_CUTOFF"
MAYU_EVALUE_CUTOFF=`echo "" | awk "END {print (10.0) ^ (-$MAYU_CUTOFF) }"`

T1=`timestamp`
logTime "Mayu" `expr $T1 - $T0`
#log "mayu time taken: " `expr $T1 - $T0`

for f in `cat ../$MS_FILES`; do
        BASE=`basename $f .mzML.gz`
	DEMIX_ID=../$BASE.tsv
	exec "$FRAGGLE interpret --excludeMode=$EXCLUDE_MODE --irtMode=$IRT_MODE --useFeatureApexRT=$FEAT_APEX_RT_FLAG --eValue=$MAYU_EVALUE_CUTOFF --excludeProtPrefix=XXX --outName=${BASE}_$ORGAN --outDir=. ../$BASE.mzML.gz $DEMIX_ID $IRT_DEF"
done

exec "$FRAGGLE combine --outName=$FINAL_NAME *_$ORGAN.fragments.bin"

T2=`timestamp`
logTime "Fraggle" `expr $T2 - $T1`
#log "fraggle time taken: " `expr $T2 - $T1`

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$TRAML --diaFile=$DIA_FILE 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=y;b;m,diaRule)' stats"

exec "$TRAMLER $FINAL_NAME.fragments.bin --outName=$DECOY_TRAML --diaFile=$DIA_FILE stats 'decoy(decoy-factor=1)' stats 'ms1-isotopes(mode=existing,n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=y;b;m,diaRule)' stats"


T3=`timestamp`
logTime "Tramler" `expr $T3 - $T2`
#log "tramler time taken: " `expr $T3 - $T2`

cd ..

