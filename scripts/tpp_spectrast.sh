#!/bin/bash

IRTs=NatProt-empirical
WORKFLOW_BASE=tpp_spectrast
SWATH_DEF_PATH=/mnt/simon/fraggle_paper/swaths.txt
IRT_DEF=/mnt/common/RTpeptides/spectrast/iRT_${IRTs}.spectrast.tsv
DATABASE=/mnt/simon/database/uniprot_refProteome_2015_10_MOUSE_RT.DECOY.fasta

if [[ $# -eq 0 ]] ; then
	echo "provide ORGAN [iRT-DEF] [DATABASE]"
	echo "this script is to be executed in a directory containing"
        echo "  all mzML.gz files of interest"
        echo "  1 iProphet.pep.xml corresponding to the TPP search result of the mzML files of interest"
        echo ""
        echo "it further assumed the existence of"
        echo "  $SWATH_DEF_PATH"
        echo "  $IRT_DEF"
        echo "  $DATABASE"
	exit 1
fi

if [[ $# -ge 2 ]] ; then
        IRTs=$2
	IRT_DEF=/mnt/common/RTpeptides/spectrast/iRT_${IRTs}.spectrast.tsv
fi
if [[ $# -ge 3 ]] ; then
        DATABASE=$3
fi

WORKFLOW=${WORKFLOW_BASE}_${IRTs}

echo ""
echo "The script will store all results the directory $WORKFLOW"

ORGAN=$1
IPROPH=iProphet_$ORGAN.pep.xml
FULL_IPROPH=$PWD/$IPROPH
FINAL_NAME=${ORGAN}-${WORKFLOW}
MAYU_PSM=${FINAL_NAME}_psm_protFDR0.01_t_1.07.csv
CONSENSUS=${FINAL_NAME}.consensus
RAW_TRAML=${FINAL_NAME}.raw.traml
RAW_DECOY_TRAML=${FINAL_NAME}.raw.decoy.traml
TRAML=${FINAL_NAME}.traml
DECOY_TRAML=${FINAL_NAME}.decoy.traml
LOG_FILE="${FINAL_NAME}.log.txt"
TIMING_FILE="${FINAL_NAME}.time.tsv"
#MAYU_RES_PARSER=../mayuProtFDR2ScoreCutoff

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
ln -s ../$IPROPH $IPROPH

T0=`timestamp`

exec "Mayu.pl -A $IPROPH -C $DATABASE -E DECOY -G 0.01 -H 51 -I 2 -v -P protFDR=0.01:t -M $FINAL_NAME"

#LAST_MAYU_OUTPUT=`ls -c *psm_protFDR*_1.07.csv | head -n 1`
log "used mayu output file '$MAYU_PSM'"

MAYU_CUTOFF=`tail -n +2 $MAYU_PSM | awk -F ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' | awk -F ',' '{print $5}'`
log "MAYU_CUTOFF = tail -n +2 $MAYU_PSM | awk -F" ',' 'NR == 1 || $5 < min {line = $0; min = $5}END{print line}' "| awk -F" ',' '{print $5}'
log "MAYU_CUTOFF = $MAYU_CUTOFF"

T1=`timestamp`
logTime "Mayu" `expr $T1 - $T0`

exec "spectrast -cN${FINAL_NAME} -cIHCD -cf'Protein!~DECOY' -cP$MAYU_CUTOFF -c_IRT$IRT_DEF -c_IRR $FULL_IPROPH"

exec "spectrast -cN$CONSENSUS -cIHCD -cAC ${FINAL_NAME}.splib"

exec "spectrast2tsv.py -l 350,2000 -s b,y -x 1,2 -o 3 -n 6 -p 0.05 -d -e -w $SWATH_DEF_PATH -k openswath -a $CONSENSUS.csv $CONSENSUS.sptxt"

T2=`timestamp`
logTime "SpectraST" `expr $T2 - $T1`

exec "ConvertTSVToTraML -in $CONSENSUS.csv -out $RAW_TRAML"

exec "OpenSwathDecoyGenerator -in $RAW_TRAML -out $RAW_DECOY_TRAML -method shuffle -exclude_similar"

T3=`timestamp`
logTime "SpectraST2Traml" `expr $T3 - $T2`

exec "java -jar /home/johant/diana2/Tramler-0.7.0.free.jar --diaFile=$SWATH_DEF_PATH --verbose --attemptKnownFixes --outName=$TRAML $RAW_TRAML 'ms1-isotopes(n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=b;y,diaRule)' stats"
exec "java -jar /home/johant/diana2/Tramler-0.7.0.free.jar --diaFile=$SWATH_DEF_PATH --verbose --attemptKnownFixes --outName=$DECOY_TRAML $RAW_DECOY_TRAML 'ms1-isotopes(n=3)' stats 'trim(fragmentMz=350:2000,nFragments=3:6,fragmentTypes=b;y,diaRule)' stats"

cd ..
