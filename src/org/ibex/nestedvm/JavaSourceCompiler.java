// Copyright 2000-2005 the Contributors, as shown in the revision logs.
// Licensed under the Apache License 2.0 ("the License").
// You may not use this file except in compliance with the License.

package org.ibex.nestedvm;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import org.ibex.nestedvm.util.ELF;
import static org.ibex.nestedvm.util.Mips.OPC_ADDI;
import static org.ibex.nestedvm.util.Mips.OPC_ADDIU;
import static org.ibex.nestedvm.util.Mips.OPC_ANDI;
import static org.ibex.nestedvm.util.Mips.OPC_BEQ;
import static org.ibex.nestedvm.util.Mips.OPC_BGTZ;
import static org.ibex.nestedvm.util.Mips.OPC_BLEZ;
import static org.ibex.nestedvm.util.Mips.OPC_BNE;
import static org.ibex.nestedvm.util.Mips.OPC_BRANCH;
import static org.ibex.nestedvm.util.Mips.OPC_J;
import static org.ibex.nestedvm.util.Mips.OPC_JAL;
import static org.ibex.nestedvm.util.Mips.OPC_ORI;
import static org.ibex.nestedvm.util.Mips.OPC_R_TYPE;
import static org.ibex.nestedvm.util.Mips.OPC_SLTI;
import static org.ibex.nestedvm.util.Mips.OPC_SLTIU;
import static org.ibex.nestedvm.util.Mips.OPC_XORI;
import static org.ibex.nestedvm.util.Mips.OP_COPR2;
import static org.ibex.nestedvm.util.Mips.OP_COPR3;
import static org.ibex.nestedvm.util.Mips.OP_FPU;
import static org.ibex.nestedvm.util.Mips.OP_LB;
import static org.ibex.nestedvm.util.Mips.OP_LBU;
import static org.ibex.nestedvm.util.Mips.OP_LH;
import static org.ibex.nestedvm.util.Mips.OP_LHU;
import static org.ibex.nestedvm.util.Mips.OP_LL;
import static org.ibex.nestedvm.util.Mips.OP_LUI;
import static org.ibex.nestedvm.util.Mips.OP_LW;
import static org.ibex.nestedvm.util.Mips.OP_LWC1;
import static org.ibex.nestedvm.util.Mips.OP_LWL;
import static org.ibex.nestedvm.util.Mips.OP_LWR;
import static org.ibex.nestedvm.util.Mips.OP_SB;
import static org.ibex.nestedvm.util.Mips.OP_SH;
import static org.ibex.nestedvm.util.Mips.OP_SW;
import static org.ibex.nestedvm.util.Mips.OP_SWC1;
import static org.ibex.nestedvm.util.Mips.OP_SWL;
import static org.ibex.nestedvm.util.Mips.OP_SWR;
import static org.ibex.nestedvm.util.Mips.OP_TLB;
import static org.ibex.nestedvm.util.Mips.RT_BRANCH_BGEZ;
import static org.ibex.nestedvm.util.Mips.RT_BRANCH_BGEZAL;
import static org.ibex.nestedvm.util.Mips.RT_BRANCH_BLTZ;
import static org.ibex.nestedvm.util.Mips.RT_BRANCH_BLTZAL;
import static org.ibex.nestedvm.util.Mips.RT_FPU_BC1;
import static org.ibex.nestedvm.util.Mips.RT_FPU_CFC1;
import static org.ibex.nestedvm.util.Mips.RT_FPU_CTC1;
import static org.ibex.nestedvm.util.Mips.RT_FPU_DOUBLE;
import static org.ibex.nestedvm.util.Mips.RT_FPU_INTEGER;
import static org.ibex.nestedvm.util.Mips.RT_FPU_MFC1;
import static org.ibex.nestedvm.util.Mips.RT_FPU_MTC1;
import static org.ibex.nestedvm.util.Mips.RT_FPU_SINGLE;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_INTEGER_CVTDW;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_INTEGER_CVTSW;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_ABSD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_ABSS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_ADDD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_ADDS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CEQD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CEQS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CLED;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CLES;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CLTD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CLTS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CVTDS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CVTWD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CVTWS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_DIVD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_DIVS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_MOVD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_MOVS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_MULD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_MULS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_NEGD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_NEGS;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_SUBD;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_SUBS;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_ADD;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_ADDU;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_AND;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_BREAK;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_DIV;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_DIVU;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_JALR;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_JR;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MFHI;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MFLO;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MTHI;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MTLO;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MULT;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_MULTU;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_NOR;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_OR;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SLL;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SLLV;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SLT;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SLTU;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SRA;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SRAV;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SRL;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SRLV;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SUB;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SUBU;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_SYSCALL;
import static org.ibex.nestedvm.util.Mips.SUB_R_TYP_XOR;
import org.ibex.nestedvm.util.Seekable;
import static org.ibex.nestedvm.util.Mips.OP_SC;
import static org.ibex.nestedvm.util.Mips.SUB_FPU_SINGLE_CVTSD;

/**
 * Compile the Mips binary to Java "like" source code
 */
public class JavaSourceCompiler extends Compiler {
    /** Stores the "case r XXX: ... run_YYYY();" blocks generated by the emitText method/ */
    private final StringBuffer runs = new StringBuffer();
    /** Stores the "initData" and "cleadData" calls generated by the emitData and emitBSS methods */
    private final StringBuffer inits = new StringBuffer();
    /** Stores lines to go in the class scope */
    private final StringBuffer classLevel = new StringBuffer();

    /** The stream to write the compiled output to */
    private final PrintWriter out;

    /** 
     * Prints a blank line to the output stream 
     */
    private void p() { 
        out.println(); 
    }
      
    /** 
     * Prints the given string (indented by <i>indent</i>*4 spaces) to the output stream 
     * 
     * @param s the string
     */
    private void p(String s) { 
        out.println(indents[indent] + s); 
    }
    
    /**
     * Print a block to the output stream
     * 
     * @param sb the block string
     */
    private void pblock(StringBuffer sb) { 
        out.print(sb.toString()); 
    }

    /** Used by the p() method to add indentation */
    private int indent;

    /** Indent in group of four */
    private static final String indents[] = new String[16];    
    static { 
      String s=""; 
      for(int i=0;i<indents.length;i++,s=s+"    ") 
          indents[i] = s; 
    }

    /**
     * Construct the complier
     * 
     * @param binary the binary input
     * @param className name of the class
     * @param w writer for output
     * @throws IOException In case of IO error
     */
    public JavaSourceCompiler(Seekable binary, String className, Writer w)  throws IOException {
        super(binary,className);
        out = new PrintWriter(w);
    }

    @Override
    protected void _go() throws Exn, IOException {
        if(singleFloat) throw new Exn("JavaSourceCompiler doesn't support singleFloat");
        String packageName;
        String className;
        
        if (fullClassName.indexOf('.') != -1) {
            packageName = fullClassName.substring(0, fullClassName.lastIndexOf('.'));
            className = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);
        } else {
            className = fullClassName;
            packageName = null;
        }

        p("/* This file was generated from " + source + " by Mips2Java on " + dateTime() + " */");
        if (packageName != null) p("package " + packageName + ";");
        if(runtimeStats) p("import java.util.*;");
        p();
        p("public final class " + className + " extends " + runtimeClass + " {");
        indent++;

        p("/* program counter */");
        p("private int pc = 0;");
        if(debugCompiler)
            p("private int lastPC = 0;");
        p();
        p("/* General Purpose registers */");
        p("private final static int r0 = 0;");
        p("private int      r1,  r2,  r3,  r4,  r5,  r6,  r7,");
        p("            r8,  r9,  r10, r11, r12, r13, r14, r15,");
        p("            r16, r17, r18, r19, r20, r21, r22, r23,");
        p("            r24, r25, r26, r27, r28, r29, r30, r31,");
        p("            hi = 0, lo = 0;");
        p("/* FP registers */");
        p("private int f0,  f1,  f2,  f3,  f4,  f5,  f6,  f7,");
        p("            f8,  f9,  f10, f11, f12, f13, f14, f15,");
        p("            f16, f17, f18, f19, f20, f21, f22, f23,");
        p("            f24, f25, f26, f27, f28, f29, f30, f31;");
        p("/* FP Control Register */");
        p("private int fcsr = 0;");
        p();

        if (onePage) p("private final int[] page = readPages[0];");

        // Generate main body functions (run_XXXX() blocks, _data[] arrays, etc)
        int highestAddr = 0;

        for (int i=0; i<elf.sheaders.length; i++) {
            ELF.SHeader sheader = elf.sheaders[i];
            String name = sheader.name;
            // if this section doesn't get loaded into our address space don't worry about it
            if (sheader.addr == 0x0) continue;

            highestAddr = Math.max(highestAddr, sheader.addr + sheader.size);

            if(name.equals(".text"))
                emitText(sheader.addr, new DataInputStream(sheader.getInputStream()),sheader.size);
            else if (name.equals(".data") || name.equals(".sdata") || 
                     name.equals(".rodata") || name.equals(".ctors") ||
                     name.equals(".dtors"))
                   emitData(sheader.addr, new DataInputStream(sheader.getInputStream()), sheader.size,name.equals(".rodata"));
            else if (name.equals(".bss") || name.equals(".sbss"))
                    emitBSS(sheader.addr,sheader.size);
            else if (name.equals(".rel.dyn") && isSectionEmpty(elf, i)) { }
            else if (name.equals(".MIPS.abiflags")) {}
            else
                throw new Exn("Unknown segment: " + name);
        }
        p();

        pblock(classLevel);
        p();

        // Trampoline (dispatch calls to the appropriate run_XXX() methods
        p("private final void trampoline() throws ExecutionException {");
        indent++;
        p("while(state == RUNNING) {");
        indent++;
        p("switch(pc>>>" + methodShift+ ") {");
        //p("switch(pc&" + toHex(methodMask) + ") {");
        indent++;
        pblock(runs);
        p("default: throw new ExecutionException(\"invalid address 0x\" + Long.toString(this.pc&0xffffffffL,16) + \": r2: \" + r2);");
        indent--; p("}");
        indent--; p("}");
        indent--; p("}");
        p();

        // Constructor
        p("public " + className + "() {");
        indent++;
        p("super(" + pageSize + "," + totalPages + ");");
        pblock(inits);
        indent--;
        p("}");
        p();

        p("protected int entryPoint() { return " + toHex(elf.header.entry) + "; }");
        p("protected int heapStart() { return " + toHex(highestAddr) + "; }");
        p("protected int gp() { return " + toHex(gp.addr) + "; }");
        if(userInfo != null) {
            p("protected int userInfoBase() { return " + toHex(userInfo.addr) + "; }");
            p("protected int userInfoSize() { return " + toHex(userInfo.size) + "; }");
        }

        // main() function
        p("public static void main(String[] args) throws Exception {");
        indent++;
        p("" + className + " me = new " + className + "();");
        p("int status = me.run(\"" + fullClassName + "\",args);");
        if(runtimeStats) p("me.printStats();");
        p("System.exit(status);");
        indent--;
        p("}");
        p();

        // Runtime abstract methods
        p("protected void _execute() throws ExecutionException { trampoline(); }");
        p();

        p("protected void setCPUState(CPUState state) {");
        indent++;
        for(int i=1;i<32;i++) p("r" + i + "=state.r[" + i + "];");
        for(int i=0;i<32;i++) p("f" + i + "=state.f[" + i + "];");
        p("hi=state.hi; lo=state.lo; fcsr=state.fcsr;");
        p("pc=state.pc;");
        indent--;
        p("}");
        p("protected void getCPUState(CPUState state) {");
        indent++;
        for(int i=1;i<32;i++) p("state.r[" + i + "]=r" + i+ ";");
        for(int i=0;i<32;i++) p("state.f[" + i + "]=f" + i +";");
        p("state.hi=hi; state.lo=lo; state.fcsr=fcsr;");
        p("state.pc=pc;");
        indent--;
        p("}");
        p();

        if(supportCall) {
            p("private static final " + hashClass + " symbols = new " + hashClass + "();");
            p("static {");
            indent++;
            ELF.Symbol[] symbols = elf.getSymtab().symbols;
            
            for (ELF.Symbol s : symbols) {
                if (s.type == ELF.Symbol.STT_FUNC && 
                    s.binding == ELF.Symbol.STB_GLOBAL && 
                    (s.name.equals("_call_helper") || !s.name.startsWith("_")))
                    p("symbols.put(\"" + s.name + "\",new Integer(" + toHex(s.addr) + "));");
            }
            indent--;
            p("}");
            p("public int lookupSymbol(String symbol) { Integer i = (Integer) symbols.get(symbol); return i==null ? -1 : i.intValue(); }");
            p();
        }

        // Runtime stats
        if(runtimeStats) {
            p("private HashMap counters = new HashMap();");
            p("private void inc(String k) { Long i = (Long)counters.get(k); counters.put(k,new Long(i==null ? 1 : i.longValue() + 1)); }");
            p("private void printStats() {");
            p(" Iterator i = new TreeSet(counters.keySet()).iterator();");
            p(" while(i.hasNext()) { Object o = i.next(); System.err.println(\"\" + o + \": \" + counters.get(o)); }");
            p("}");
            p();
        }

        indent--;
        p("}");
    }

    /** Address of the start of a method */
    private int startOfMethod = 0;
    
    /** Address of the end of a method */
    private int endOfMethod = 0;

    /**
     * Emit the text for start of a method at the given address (used for name too)
     * 
     * @param addr address to use
     */
    private void startMethod(int addr) {
        addr &= ~(maxBytesPerMethod-1);
        startOfMethod = addr;
        endOfMethod = addr + maxBytesPerMethod;
        String methodName = "run_" + Long.toString(addr & 0xffffffffL, 16);
        runs.append(indents[4] + "case " + toHex(addr>>>methodShift) + ": " + methodName + "(); break; \n");
        //runs.append(indents[4] + "case " + toHex(addr&methodMask) + ": " + methodName + "(); break; \n");

        p("private final void " + methodName + "() throws ExecutionException { /"+"* " + toHex(addr) + " - " + toHex(endOfMethod) + " *" + "/");
        indent++;
        p("int addr, tmp;");
        p("for(;;) {");
        indent++;
        p("switch(pc) {");
        indent++;
    }

    /**
     * Emit the text for the end of a method (using the last address)
     */
    private void endMethod() { 
        endMethod(endOfMethod);
    }
    
    /**
     * Emit the text for end of a method with the given address
     * 
     * @param lastAddr the last address of the method
     */
    private void endMethod(int lastAddr) {
        if (startOfMethod == 0) return;
        
        // We should be able to use if(!unreachable) here (i think)
        // This isn't strictly necessary; its just here to work around unreachable code errors
        p("case " + toHex(lastAddr) + ":");
        indent++;
        p("pc=" + constant(lastAddr) + ";");
        leaveMethod();
        indent--;
        if(debugCompiler)
            p("default: throw new ExecutionException(\"invalid address 0x\" + Long.toString(pc&0xffffffffL,16)  + \" (got here from 0x\" + Long.toString(lastPC&0xffffffffL,16)+\")\");");
        else
            p("default: throw new ExecutionException(\"invalid address 0x\" + Long.toString(pc&0xffffffffL,16));");
        indent--;
        p("}"); // end switch
        p("/* NOT REACHED */");
        indent--;
        p("}"); // end for
        indent--;
        p("}"); // end method
        endOfMethod = startOfMethod = 0;
    }

    /** Map for relative address in branch */
    private final HashMap<Integer,Boolean> relativeAddrs = new HashMap<>();
    
    /**
     * Generate a text for a constant used in branch
     * 
     * @param target the target of the branch
     * @return the text
     */
    private String constant(int target) {
        if (target >= 4096 && lessConstants) {
            int n = target & ~1023;
            String var = "N_" + toHex8(n);
            if (relativeAddrs.get(n) == null) {
                relativeAddrs.put(n, Boolean.TRUE);
                classLevel.append(indents[1] + "private static int " + var + " = " + toHex(n) + ";\n");
            }
            return "(" + var + " + " + toHex(target - n) + ")";
        } else {
            return toHex(target);
        }
    }

    /**
     * Emit the text for a branch 
     * 
     * @param pc the last program counter
     * @param target the target program counter
     */
    private void branch(int pc, int target) {
        if (debugCompiler) p("lastPC = " + toHex(pc) + ";");
        p("pc=" + constant(target) + ";");
        if(target == 0)
            p("throw new ExecutionException(\"Branch to addr 0x0\");");
        else if((pc&methodMask) == (target&methodMask))
            p("continue;");
        else if(assumeTailCalls)
            p("run_" +  Long.toString((target&methodMask)&0xffffffffL, 16) + "(); return;");
        else
            leaveMethod();
    }

    /** 
     * Emit text for leaving a method
     */
    private void leaveMethod() {
        p("return;");
    }

    private boolean textDone;
    
    /**
     * Emit the text for a text section at the given address
     * 
     * @param addr the address
     * @param dis the input stream with data
     * @param size the size of the block
     * @throws org.ibex.nestedvm.Compiler.Exn In case of compilation error
     * @throws IOException In case of IO error
     */
    private void emitText(int addr, DataInputStream dis, int size) throws Exn,IOException {
        if (textDone) throw new Exn("Multiple text segments");
        textDone = true;

        if ((addr&3)!=0 || (size&3)!=0) throw new Exn("Section on weird boundaries");
        int count = size/4;
        int nextInsn = dis.readInt();
        if (nextInsn == -1) throw new Error("Actually read -1 at " + toHex(addr));
        int insn;

        for(int i=0; i<count; i++, addr+=4) {
            insn = nextInsn;
            nextInsn = (i == count-1) ? -1 : dis.readInt();
            
            if(addr >= endOfMethod) { 
                endMethod(); 
                startMethod(addr); 
            }
            
            if (jumpableAddresses==null || addr == startOfMethod || jumpableAddresses.get(addr) != null) {
                p("case " + toHex(addr) + ":");
                unreachable = false;
            } else if(unreachable) {
                continue;
            } else if(debugCompiler) {
                p("/" + "* pc = " + toHex(addr) + "*" + "/");
            }
            indent++;
            emitInstruction(addr,insn,nextInsn);
            indent--;
        }
        endMethod(addr);
        p();
        dis.close();
    }

    private int initDataCount = 0;
    
    /**
     * Emit the text for a data section at the given address
     * 
     * @param addr the address
     * @param dis the input stream with data
     * @param size the size of the block
     * @param readOnly true for readonly
     * @throws org.ibex.nestedvm.Compiler.Exn In case of compilation error
     * @throws IOException In case of IO error
     */
    private void emitData(int addr, DataInputStream dis, int size, boolean readOnly) throws Exn,IOException {
        if ((addr&3)!=0 || (size&3)!=0) throw new Exn("Data section on weird boundaries");
        
        int last = addr + size;
        while(addr < last) {
            int segSize = Math.min(size,28000); // must be a multiple of 56
            StringBuffer sb = new StringBuffer();
            for(int i=0;i<segSize;i+=7) {
                long l = 0;
                for(int j=0;j<7;j++) {
                    l <<= 8;
                    byte b = (i+j < size) ? dis.readByte() : 1;
                    l |= (b & 0xffL);
                }
                for(int j=0;j<8;j++) {
                    char c = (char) ((l>>>(7*(7-j)))&0x7f);
                    if(c=='\n') sb.append("\\n");
                    else if(c=='\r') sb.append("\\r");
                    else if(c=='\\') sb.append("\\\\");
                    else if(c=='"') sb.append("\\\"");
                    else if(c >= 32 && c <= 126) sb.append(c);
                    else sb.append("\\" +  toOctal3(c));
                }
            }
            String varname =  "_data" + (++initDataCount);
            p("private static final int[] " + varname + " = decodeData(\"" + sb.toString() + "\"," + toHex(segSize/4) + ");");
            inits.append(indents[2] + "initPages(" + varname +"," + toHex(addr) + "," + (readOnly?"true":"false") + ");\n");
            addr += segSize;
            size -= segSize;
        }
        dis.close();
    }

    /**
     * Emit the text for a BBS section at the given address
     * 
     * @param addr the address of section
     * @param size the size of section
     * @throws org.ibex.nestedvm.Compiler.Exn In case of compilation error
     */
    private void emitBSS(int addr, int size) throws Exn {
        if ((addr&3)!=0) throw new Exn("BSS section on weird boundaries");
        
        size = (size+3)&~3;
        int count = size/4;
        inits.append(indents[2] + "clearPages(" + toHex(addr) + "," + toHex(count) + ");\n");
    }

    // True if the current code path is unreachable (any instruction with a case statement is reachable)
    private boolean unreachable = false;

    /**
     * Emit an instruction 
     * 
     * @param pc program counter
     * @param insn the instruction
     * @param nextInsn the next instruction
     * @throws IOException In case of IO error
     * @throws org.ibex.nestedvm.Compiler.Exn In case of compilation error
     */
    private void emitInstruction(int pc, int insn, int nextInsn) throws IOException,Exn {
        if(insn == -1) throw new Error("insn is -1");

        int op = (insn >>> 26) & 0xff;                 // bits 26-31
        int rs = (insn >>> 21) & 0x1f;                 // bits 21-25
        int rt = (insn >>> 16) & 0x1f;                 // bits 16-20
        int ft = (insn >>> 16) & 0x1f;
        int rd = (insn >>> 11) & 0x1f;                 // bits 11-15
        int fs = (insn >>> 11) & 0x1f;
        int shamt = (insn >>> 6) & 0x1f;               // bits 6-10
        int fd = (insn >>> 6) & 0x1f;
        int subcode = insn & 0x3f;                     // bits 0-5

        int jumpTarget = (insn & 0x03ffffff);          // bits 0-25
        int unsignedImmediate = insn & 0xffff;
        int signedImmediate = (insn << 16) >> 16;
        int branchTarget = signedImmediate;

        int tmp; // temporaries

        //if(pc%64==0) p("System.err.println(\"Executing: " + toHex(pc) + "\");");
        //p("/" + "*" + (pc == -1 ? "Delay Slot"  : toHex(pc)) + " *" + "/ ");
        if (pc==-1) p("/" + "* Next insn is delay slot *" + "/ ");

        if (runtimeStats && op != 0) p("inc(\"opcode: " + op + "\");");
        switch(op) {
            case OPC_R_TYPE: {   // R-type instruction
                if (runtimeStats && insn != 0) p("inc(\"opcode: 0/" + subcode + "\");");
                switch(subcode) {
                    case SUB_R_TYP_SLL:  // SLL (Shit Left Logical)
                        if (insn != 0)
                            p( "r"+rd+" = r"+rt+" << "+shamt+";");
                        break;
                    case SUB_R_TYP_SRL:  // SRL (Shit Right Logical)
                        p( "r"+rd+" = r"+rt+" >>> "+shamt+";");
                        break;
                    case SUB_R_TYP_SRA:  // SRA (Shift Right Aritmetic)
                        p( "r"+rd+" = r"+rt+" >> "+shamt+";");
                        break;
                    case SUB_R_TYP_SLLV: // SLLV (Shift Left Logical Variable)
                        p( "r"+rd+" = r"+rt+" << (r"+rs+"&0x1f);");
                        break;
                    case SUB_R_TYP_SRLV: // SRLV (Shift Right Logical Variable)
                        p( "r"+rd+" = r"+rt+" >>> (r"+rs+"&0x1f);");
                        break;
                    case SUB_R_TYP_SRAV: // SRAV (Shift Right Aritmetic Variable)
                        p( "r"+rd+" = r"+rt+" >> (r"+rs+"&0x1f);");
                        break;
                    case SUB_R_TYP_JR:   // JR (Jump Register)
                        if (pc == -1) throw new Error("pc modifying insn in delay slot");
                        emitInstruction(-1,nextInsn,-1);
                        if (debugCompiler) p("lastPC = " + toHex(pc) + ";");
                        p("pc=r" + rs + ";");
                        leaveMethod();
                        unreachable = true;
                        break;
                    case SUB_R_TYP_JALR: // JALR (Jump and Link Register)
                        if (pc == -1) throw new Error("pc modifying insn in delay slot");
                        emitInstruction(-1,nextInsn,-1);
                        if (debugCompiler) p("lastPC = " + toHex(pc) + ";");
                        p("pc=r" + rs + ";");
                        p("r" + RA + "=" + constant(pc+8 /*skip this insn and delay slot*/) + ";");
                        leaveMethod();
                        unreachable = true;
                        break;
                    case SUB_R_TYP_SYSCALL: // SYSCALL (System call)
                        p("pc = " + toHex(pc) + ";");
                        p( "r"+V0+" = syscall(r"+V0+",r"+A0+",r"+A1+",r"+A2+",r"+A3+",r"+T0+",r"+T1+");");
                        p("if (state != RUNNING) {");
                            indent++;
                            p("pc = " + toHex(pc+4) + ";");
                            leaveMethod();
                            indent--;
                        p("}");
                        break;
                    case SUB_R_TYP_BREAK:   // BREAK
                        p( "throw new ExecutionException(\"Break\");");
                        unreachable = true;
                        break;
                    case SUB_R_TYP_MFHI: // MFHI (Move From HI)
                        p( "r"+rd+" = hi;");
                        break;
                    case SUB_R_TYP_MTHI: // MTHI (Move To HI)
                        p( "hi = r"+rs+";");
                        break;
                    case SUB_R_TYP_MFLO: // MFLO (Move From LO)
                        p( "r"+rd+" = lo;");
                        break;
                    case SUB_R_TYP_MTLO: // MTLO (Move To LO)
                        p( "lo = r"+rs+";");
                        break;
                    case SUB_R_TYP_MULT: // MULT (MULTiply)
                        p( "{ long hilo = (long)(r"+rs+") * ((long)r"+rt+"); " +
                             "hi = (int) (hilo >>> 32); " +
                             "lo = (int) hilo; }");
                        break;
                    case SUB_R_TYP_MULTU: // MULTU (MULTiply Unsigned)
                        p( "{ long hilo = (r"+rs+" & 0xffffffffL) * (r"+rt+" & 0xffffffffL); " +
                             "hi = (int) (hilo >>> 32); " +
                             "lo = (int) hilo; } ");
                        break;
                    case SUB_R_TYP_DIV: // DIV (DIVision)
                        p( "hi = r"+rs+"%r"+rt+"; lo = r"+rs+"/r"+rt+";");
                        break;
                    case SUB_R_TYP_DIVU: // DIVU (DIVision Unsigned)
                        p("if(r"+rt+"!=0) {");
                        p( "hi = (int)((r"+rs+" & 0xffffffffL) % (r"+rt+" & 0xffffffffL)); " +
                             "lo = (int)((r"+rs+" & 0xffffffffL) / (r"+rt+" & 0xffffffffL));");
                        p("}");
                        break;
                    case SUB_R_TYP_ADD: // ADD (ADDition)
                         throw new Exn("ADD (add with oveflow trap) not suported");
                        /*This must trap on overflow
                        p( "r"+rd+" = r"+rs+" + r"+rt+";");
                        break;*/
                    case SUB_R_TYP_ADDU: // ADDU (ADDition Unsigned)
                        p( "r"+rd+" = r"+rs+" + r"+rt+";");
                        break;
                    case SUB_R_TYP_SUB: // SUB (SUBtraction)
                         throw new Exn("SUB (add with oveflow trap) not suported");
                        /*This must trap on overflow
                        p( "r"+rd+" = r"+rs+" - r"+rt+";");
                        break;*/
                    case SUB_R_TYP_SUBU: // SUBU (SUBtraction Unsigned)
                        p( "r"+rd+" = r"+rs+" - r"+rt+";");
                        break;
                    case SUB_R_TYP_AND: // AND
                        p( "r"+rd+" = r"+rs+" & r"+rt+";");
                        break;
                    case SUB_R_TYP_OR: // OR
                        p( "r"+rd+" = r"+rs+" | r"+rt+";");
                        break;
                    case SUB_R_TYP_XOR: // XOR
                        p( "r"+rd+" = r"+rs+" ^ r"+rt+";");
                        break;
                    case SUB_R_TYP_NOR: // NOR
                        p( "r"+rd+" = ~(r"+rs+" | r"+rt+");");
                        break;
                    case SUB_R_TYP_SLT: // SLT (Set on Less Than)
                        p( "r"+rd+" = r"+rs+" < r"+rt+" ? 1 : 0;");
                        break;
                    case SUB_R_TYP_SLTU: // SLTU (Set on Less Than Unsigned)
                        p( "r"+rd+" = ((r"+rs+" & 0xffffffffL) < (r"+rt+" & 0xffffffffL)) ? 1 : 0;");
                        break;
                    default:
                        throw new RuntimeException("Illegal instruction 0/" + subcode);
                }
                break;
            }
            case OPC_BRANCH: { // Branch instructions
                switch(rt) {
                    case RT_BRANCH_BLTZ: // BLTZ (Branch Less Than Zero)
                        if(pc == -1) throw new Error("pc modifying insn in delay slot");
                        p("if(r" + rs + " < 0) {");
                            indent++;
                            emitInstruction(-1,nextInsn,-1);
                            branch(pc,pc+branchTarget*4+4);
                            indent--;
                        p("}");
                        break;
                    case RT_BRANCH_BGEZ: // BGEZ (Branch Greater Equal Zero)
                        if(pc == -1) throw new Error("pc modifying insn in delay slot");
                        p("if(r" + rs + " >= 0) {");
                            indent++;
                            emitInstruction(-1,nextInsn,-1);
                            branch(pc,pc+branchTarget*4+4);
                            indent--;
                        p("}");
                        break;
                    case RT_BRANCH_BLTZAL: // BLTZAL (Branch Less Than Zero And Link)
                        if(pc == -1) throw new Error("pc modifying insn in delay slot");
                        p("if(r" + rs + " < 0) {");
                            indent++;
                            emitInstruction(-1,nextInsn,-1);
                            p("r" + RA + "=" + constant(pc+8 /*skip this insn and delay slot*/) + ";");
                            branch(pc,pc+branchTarget*4+4);
                            indent--;
                        p("}");
                        break;
                    case RT_BRANCH_BGEZAL: // BGEZAL (Branch Greater Equal Zero And Link)
                        if(pc == -1) throw new Error("pc modifying insn in delay slot");
                        p("if(r" + rs + " >= 0) {");
                            indent++;
                            emitInstruction(-1,nextInsn,-1);
                            p("r" + RA + "=" + constant(pc+8 /*skip this insn and delay slot*/) + ";");
                            branch(pc,pc+branchTarget*4+4);
                            indent--;
                        p("}");
                        break;
                    default:
                        throw new RuntimeException("Illegal Instruction 1/" + rt);
                }
                break;
            }
            case OPC_J: { // J (Jump)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                emitInstruction(-1,nextInsn,-1);
                branch(pc,(pc&0xf0000000)|(jumpTarget << 2));
                unreachable = true;
                break;
            }
            case OPC_JAL: { // JAL (Jump And Link)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                int target = (pc&0xf0000000)|(jumpTarget << 2);
                emitInstruction(-1,nextInsn,-1);
                p("r" + RA + "=" + constant(pc+8 /*skip this insn and delay slot*/) + ";");
                branch(pc, target);
                unreachable = true;
                break;
            }
            case OPC_BEQ: // BEQ (Branch EQual)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                p("if(r" + rs + " == r" + rt + ") {");
                    indent++;
                    emitInstruction(-1,nextInsn,-1);
                    branch(pc,pc+branchTarget*4+4);
                    indent--;
                p("}");
                break;
            case OPC_BNE: // BNE (Branch Not Equal)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                p("if(r" + rs + " != r" + rt + ") {");
                    indent++;
                    emitInstruction(-1,nextInsn,-1);
                    branch(pc,pc+branchTarget*4+4);
                    indent--;
                p("}");
                break;
            case OPC_BLEZ: //BLEZ (Brach Less Equal Zero)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                p("if(r" + rs + " <= 0) {");
                    indent++;
                    emitInstruction(-1,nextInsn,-1);
                    branch(pc,pc+branchTarget*4+4);
                    indent--;
                p("}");
                break;
            case OPC_BGTZ: //BGTZ (Branch Greater Than Zero)
                if(pc == -1) throw new Error("pc modifying insn in delay slot");
                p("if(r" + rs + " > 0) {");
                    indent++;
                    emitInstruction(-1,nextInsn,-1);
                    branch(pc,pc+branchTarget*4+4);
                    indent--;
                p("}");
                break;
            case OPC_ADDI: // ADDI (ADD Immediate)
                p( "r"+rt+" = r"+rs+" + "+signedImmediate +";");
                break;
            case OPC_ADDIU: // ADDIU (ADD Immediate Unsigned)
                p( "r"+rt+" = r"+rs+" + "+signedImmediate+";");
                break;
            case OPC_SLTI: // SLTI (Set on Less That Immediate)
                p( "r"+rt+" = r"+rs+" < "+signedImmediate+" ? 1 : 0;");
                break;
            case OPC_SLTIU: // SLTIU (Set on Less That Immediate Unsigned)
                p( "r"+rt+" = (r"+rs+"&0xffffffffL) < ("+signedImmediate+"&0xffffffffL) ? 1 : 0;");
                break;
            case OPC_ANDI: // ANDI (AND Immediate)
                p( "r"+rt+" = r"+rs+" & "+unsignedImmediate+";");
                break;
            case OPC_ORI: // ORI (OR Immediate)
                p( "r"+rt+" = r"+rs+" | "+unsignedImmediate+";");
                break;
            case OPC_XORI: // XORI (XOR immediate)
                p( "r"+rt+" = r"+rs+" ^ "+unsignedImmediate+";");
                break;
            case OP_LUI: // LUI (Load Upper Immediate)
                p( "r"+rt+" = "+unsignedImmediate+" << 16;");
                break;
            case OP_TLB: // TLB (Translation Lookaside Buffer)
                throw new Exn("TLB/Exception support not implemented");
            case OP_FPU: { // FPU (Floating Point Unit)
                switch(rs) {
                    case RT_FPU_MFC1: // MFC.1 (Move From Coprocessor)
                        p( "r"+rt+" = f"+rd+";");
                        break;
                    case RT_FPU_CFC1: // CFC.1 (Copy From Coprocessor)
                        if (fs != 31) throw new Exn("FCR " + fs + " unavailable");
                        p( "r"+rt+" = fcsr;");
                        break;
                    case RT_FPU_MTC1: // MTC.1 (Move To Coprocessor)
                        p( "f"+rd+" = r"+rt+";");
                        break;
                    case RT_FPU_CTC1: // CTC.1 (Copy To coprocessor)
                        if (fs != 31) throw new Exn("FCR " + fs + " unavailable");
                        p( "fcsr = r"+rt+";");
                        break;
                    case RT_FPU_BC1: {// BC1F, BC1T (Branch on Coprocessor)
                        tmp = (insn>>>16)&1;
                        p("if(((fcsr&0x800000)!=0) == (" + tmp + "!=0)) {");
                            indent++;
                            emitInstruction(-1,nextInsn,-1);
                            branch(pc,pc+branchTarget*4+4);
                            indent--;
                        p("}");
                        break;
                    }
                    case RT_FPU_SINGLE: {  // Single
                        switch(subcode) {
                            case SUB_FPU_SINGLE_ADDS: // ADD.S
                                p(setFloat(fd,getFloat(fs)+"+"+getFloat(ft)));
                                break;
                            case SUB_FPU_SINGLE_SUBS: // SUB.S
                                p(setFloat(fd,getFloat(fs)+"-"+getFloat(ft)));
                                break;
                            case SUB_FPU_SINGLE_MULS: // MUL.S
                                p(setFloat(fd,getFloat(fs)+"*"+getFloat(ft)));
                                break;
                            case SUB_FPU_SINGLE_DIVS: // DIV.S
                                p(setFloat(fd,getFloat(fs)+"/"+getFloat(ft)));
                                break;
                            case SUB_FPU_SINGLE_ABSS: // ABS.S
                                p(setFloat(fd,"Math.abs("+getFloat(fs)+")"));
                                break;
                            case SUB_FPU_SINGLE_MOVS: // MOV.S
                                p("f"+fd+" = f"+fs+"; // MOV.S");
                                break;
                            case SUB_FPU_SINGLE_NEGS: // NEG.S
                                p(setFloat(fd,"-"+getFloat(fs)));
                                break;
                            case SUB_FPU_SINGLE_CVTDS: // CVT.D.S
                                p(setDouble(fd,"(float)"+getFloat(fs)));
                                break;
                            case SUB_FPU_SINGLE_CVTWS: // CVT.W.S
                                p("switch(fcsr & 3) {");
                                    indent++;
                                    p("case 0: f"+fd+" = (int)Math.floor("+getFloat(fs)+"+0.5); break; // Round to nearest");
                                    p("case 1: f"+fd+" = (int)"+getFloat(fs)+"; break; // Round towards zero");
                                    p("case 2: f"+fd+" = (int)Math.ceil("+getFloat(fs)+"); break; // Round towards plus infinity");
                                    p("case 3: f"+fd+" = (int)Math.floor("+getFloat(fs)+"); break; // Round towards minus infinity");
                                    indent--;
                                p("}");
                                break;
                            case SUB_FPU_SINGLE_CEQS: // C.EQ.S
                                p("fcsr = (fcsr&~0x800000) | (("+getFloat(fs)+"=="+getFloat(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            case SUB_FPU_SINGLE_CLTS: // C.LT.S
                                p("fcsr = (fcsr&~0x800000) | (("+getFloat(fs)+"<"+getFloat(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            case SUB_FPU_SINGLE_CLES: // C.LE.S
                                p("fcsr = (fcsr&~0x800000) | (("+getFloat(fs)+"<="+getFloat(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            default: throw new Exn("Invalid Instruction 17/" + rs + "/" + subcode);
                        }
                        break;
                    }
                    case RT_FPU_DOUBLE: { // Double
                        switch(subcode) {
                            case SUB_FPU_SINGLE_ADDD: // ADD.D
                                p(setDouble(fd,getDouble(fs)+"+"+getDouble(ft)));
                                break;
                            case SUB_FPU_SINGLE_SUBD: // SUB.D
                                p(setDouble(fd,getDouble(fs)+"-"+getDouble(ft)));
                                break;
                            case SUB_FPU_SINGLE_MULD: // MUL.D
                                p(setDouble(fd,getDouble(fs)+"*"+getDouble(ft)));
                                break;
                            case SUB_FPU_SINGLE_DIVD: // DIV.D
                                p(setDouble(fd,getDouble(fs)+"/"+getDouble(ft)));
                                break;
                            case SUB_FPU_SINGLE_ABSD: // ABS.D
                                p(setDouble(fd,"Math.abs("+getDouble(fs)+")"));
                                break;
                            case SUB_FPU_SINGLE_MOVD: // MOV.D
                                p("f"+fd+" = f"+fs+";");
                                p("f"+(fd+1)+" = f"+(fs+1)+";");
                                break;
                            case SUB_FPU_SINGLE_NEGD: // NEG.D
                                p(setDouble(fd,"-"+getDouble(fs)));
                                break;
                            case SUB_FPU_SINGLE_CVTSD: // CVT.S.D
                                p(setFloat(fd,"(float)"+getDouble(fs)));
                                break;
                            case SUB_FPU_SINGLE_CVTWD: // CVT.W.D
                                p("switch(fcsr & 3) {");
                                    indent++;
                                    p("case 0: f"+fd+" = (int)Math.floor("+getDouble(fs)+"+0.5); break; // Round to nearest");
                                    p("case 1: f"+fd+" = (int)"+getDouble(fs)+"; break; // Round towards zero");
                                    p("case 2: f"+fd+" = (int)Math.ceil("+getDouble(fs)+"); break; // Round towards plus infinity");
                                    p("case 3: f"+fd+" = (int)Math.floor("+getDouble(fs)+"); break; // Round towards minus infinity");
                                    indent--;
                                p("}");
                                break;
                            case SUB_FPU_SINGLE_CEQD: // C.EQ.D
                                p("fcsr = (fcsr&~0x800000) | (("+getDouble(fs)+"=="+getDouble(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            case SUB_FPU_SINGLE_CLTD: // C.LT.D
                                p("fcsr = (fcsr&~0x800000) | (("+getDouble(fs)+"<"+getDouble(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            case SUB_FPU_SINGLE_CLED: // C.LE.D
                                p("fcsr = (fcsr&~0x800000) | (("+getDouble(fs)+"<="+getDouble(ft)+") ? 0x800000 : 0x000000);");
                                break;
                            default: throw new Exn("Invalid Instruction 17/" + rs + "/" + subcode);
                        }
                        break;
                    }
                    case RT_FPU_INTEGER: { // Integer
                        switch(subcode) {
                            case SUB_FPU_INTEGER_CVTSW: // CVT.S.W
                                p(" // CVS.S.W");
                                p(setFloat(fd,"((float)f"+fs+")"));
                                break;
                            case SUB_FPU_INTEGER_CVTDW: // CVT.D.W
                                p(setDouble(fd,"((double)f"+fs+")"));
                                break;
                            default: throw new Exn("Invalid Instruction 17/" + rs + "/" + subcode);
                        }
                        break;
                    }
                    default:
                        throw new Exn("Invalid Instruction 17/" + rs);
                }
                break;
            }
            case OP_COPR2: 
            case OP_COPR3:
                throw new Exn("coprocessor 2 and 3 instructions not available");
            case OP_LB: { // LB (Load Byte)
                if (runtimeStats) p("inc(\"LB\");");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp>>>(((~addr)&3)<<3)) & 0xff;");
                p("if((tmp&0x80)!=0) tmp |= 0xffffff00; /* sign extend */");
                p("r"+rt+" = tmp;");
                break;
            }
            case OP_LH: { // LH (Load Halfword
                if (runtimeStats) p("inc(\"LH\");");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp>>>(((~addr)&2)<<3)) & 0xffff;");
                p("if((tmp&0x8000)!=0) tmp |= 0xffff0000; /* sign extend */");
                p("r"+rt+" = tmp;");
                break;
            }
            case OP_LWL: { // LWL (Load Word Left)
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("r" + rt + " = (r"+rt+"&(0x00ffffff>>>(((~addr)&3)<<3)))|(tmp<<((addr&3)<<3));");
                break;
                /*p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr&~3","tmp");
                p("switch(addr&3) {");
                indent++;
                p("case 0: r"+rt+" = (r"+rt+"&0x00000000)|(tmp<< 0); break;");
                p("case 1: r"+rt+" = (r"+rt+"&0x000000ff)|(tmp<< 8); break;");
                p("case 2: r"+rt+" = (r"+rt+"&0x0000ffff)|(tmp<<16); break;");
                p("case 3: r"+rt+" = (r"+rt+"&0x00ffffff)|(tmp<<24); break;");
                indent--;
                p("}");
                break;*/
            }
            case OP_LW: // LW (Load Word)
                if (runtimeStats) p("inc(\"LW\");");
                memRead("r" + rs +"+"+signedImmediate,"r"+rt);
                break;
            case OP_LBU: { // LBU (Load Byte Unsigned)
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp>>>(((~addr)&3)<<3)) & 0xff;");
                p("r"+rt+" = tmp;");
                break;
            }
            case OP_LHU: { // LHU (Load Halfword Unsigned)
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp>>>(((~addr)&2)<<3)) & 0xffff;");
                p("r"+rt+" = tmp;");
                break;
            }
            case OP_LWR: { // LWR (Load Word Right)
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("r" + rt + " = (r"+rt+"&(0xffffff00<<((addr&3)<<3)))|(tmp>>>(((~addr)&3)<<3));");
                break;

                /*p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr&~3","tmp");
                p("switch(addr&3) {");
                indent++;
                p("case 0: r"+rt+" = (r"+rt+"&0xffffff00)|(tmp>>>24); break;");
                p("case 1: r"+rt+" = (r"+rt+"&0xffff0000)|(tmp>>>16); break;");
                p("case 2: r"+rt+" = (r"+rt+"&0xff000000)|(tmp>>> 8); break;");
                p("case 3: r"+rt+" = (r"+rt+"&0x00000000)|(tmp>>> 0); break;");
                indent--;
                p("}");
                break;*/

            }
            case OP_SB: { // SB (Store Byte)
                if (runtimeStats) p("inc(\"SB\");");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp&~(0xff000000>>>((addr&3)<<3)))|((r"+rt+"&0xff)<<(((~addr)&3)<<3));");
                memWrite("addr","tmp");
                break;
            }
            case OP_SH: { // SH (Store Halfword)
                if (runtimeStats) p("inc(\"SH\");");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp&(0xffff<<((addr&2)<<3)))|((r" + rt + "&0xffff)<<(((~addr)&2)<<3));");
                memWrite("addr","tmp");
                break;
            }
            case OP_SWL: { // SWL  (Store Word Left)
                p(" // SWL");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp&(0xffffff00<<(((~addr)&3)<<3)))|(r"+rt+">>>((addr&3)<<3));");
                memWrite("addr","tmp");
                break;
            }
            case OP_SW: { // SW (Store Word)
                if (runtimeStats) p("inc(\"SW\");");
                memWrite("r"+rs+"+"+signedImmediate,"r" + rt);
                break;
            }
            case OP_SWR: { // SWR
                p(" // SWR");
                p("addr=r" + rs +"+"+signedImmediate + ";");
                memRead("addr","tmp");
                p("tmp = (tmp&(0x00ffffff>>>((addr&3)<<3)))|(r"+rt+"<<(((~addr)&3)<<3));");
                memWrite("addr","tmp");
                break;
            }
            // Need to be atomic if threads
            case OP_LL: // LWC0/LL (Load Linker)
                memRead("r"+rs+"+"+signedImmediate,"r"+rt);
                break;
            case OP_LWC1: // LWC1 (Load Word Coprocessor 1)
                memRead("r"+rs+"+"+signedImmediate,"f"+rt);
                break;
            // Needs to be atomic if threads
            case OP_SC: // SWC1/SC (Store Linker) 
                memWrite("r"+rs+"+"+signedImmediate,"r"+rt);
                p("r" + rt + "=1;");
                break;
            case OP_SWC1: // SWC1 (Store Word Coprocessor 1)
                memWrite("r"+rs+"+"+signedImmediate,"f"+rt);
                break;
            default:
                throw new Exn("Invalid Instruction: " + op + " at " + toHex(pc));
        }
    }

    // Helper functions for emitText
    // NOTE: memWrite and memRead MUST discard the last two bits of addr
    
    /**
     * Emit the text of memory write
     * 
     * @param addr the address of memory
     * @param target the value
     */
    private void memWrite(String addr, String target) {
        if (nullPointerCheck) p("nullPointerCheck(" + addr + ");");
        if (onePage)
            p("page[(" + addr + ")>>>2] = " + target + ";");
        else if(fastMem)
            p("writePages[("+addr+")>>>"+pageShift+"][(("+addr+")>>>2)&"+toHex((pageSize>>2)-1)+"] = " + target + ";");
        else
            p("unsafeMemWrite(" + addr + "," + target + ");");
    }
    
    /**
     * Emit the text of memory read
     * 
     * @param addr the address to read
     * @param target the value where to read
     */
    private void memRead(String addr, String target) {
        if (nullPointerCheck) p("nullPointerCheck(" + addr + ");");
        if (onePage)
            p(target + "= page[(" + addr + ")>>>2];");
        else if(fastMem)
            p(target  + " = readPages[("+addr+")>>>"+pageShift+"][(("+addr+")>>>2)&"+toHex((pageSize>>2)-1)+"];");
        else
            p(target + " = unsafeMemRead(" + addr + ");");
    }
    
    /**
     * Emit the text for a float
     * 
     * @param r the register
     * @return the float get string
     */
    private static String getFloat(int r) { 
        return "(Float.intBitsToFloat(f"+r+"))"; 
    }
    
    /**
     * Emit the text for a double
     * 
     * @param r the register
     * @return the float get string
     */
    private static String getDouble(int r) {
        return "(Double.longBitsToDouble(((f"+(r+1)+"&0xffffffffL) << 32) | (f"+r+"&0xffffffffL)))";
    }
    
    /**
     * Emit the text for set of a float
     * 
     * @param r the register
     * @param expr float expression
     * @return float set string
     */
    private static String setFloat(int r, String expr) { 
        return "f"+r+"=Float.floatToRawIntBits("+expr+");"; 
    }
    
    /**
     * Emit the text for set of a double
     * 
     * @param r the register
     * @param expr double expression
     * @return double set string
     */
    private static String setDouble(int r, String expr) {
        return "{ long l = Double.doubleToLongBits("+expr+"); "+
            "f"+(r+1)+" = (int)(l >>> 32); f"+r+" = (int)l; }";
    }
}

