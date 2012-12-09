OUTDIR=out

all:
	mkdir -p ${OUTDIR}
	as -o ${OUTDIR}/${asrc}.o ${asrc}.s;
	ld -o ${OUTDIR}/${asrc} ${OUTDIR}/${asrc}.o;
	
