OUTDIR=out

all:
	mkdir -p ${OUTDIR}
	as ${ASFLAGS} -o ${OUTDIR}/${asrc}.o ${asrc}.s;
	ld -o ${OUTDIR}/${asrc} ${OUTDIR}/${asrc}.o;

opt: all

debug:
	${MAKE} ASFLAGS=-gstabs
