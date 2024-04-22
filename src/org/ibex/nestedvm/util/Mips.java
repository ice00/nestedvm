package org.ibex.nestedvm.util;

/**
 * Mips opcodes 
 */
public class Mips {
    /** Opcode for R-type instructions */
    public static final int OPC_R_TYPE = 0;

    /** Opcode for branch instruction */
    public static final int OPC_BRANCH = 1;
    
    /** Opcode for jump instruction */
    public static final int OPC_J = 2;

    /** Opcode for jump and link instruction */
    public static final int OPC_JAL = 3;

    /** Opcode for branch if equal instruction */
    public static final int OPC_BEQ = 4;

    /** Opcode for branch if not equal instruction */
    public static final int OPC_BNE = 5;
    
    /** Opcode for branch on less than or equal to zero instruction */
    public static final int OPC_BLEZ = 6;
    
    /** Opcode for branch on greater than zero instruction */
    public static final int OPC_BGTZ = 7;
    
    /** Opcode for add immediate instruction */
    public static final int OPC_ADDI = 8;    

    /** Opcode for add immediate unsigned instruction */
    public static final int OPC_ADDIU = 9;
    
    /** Opcode for set less than immediate instruction */
    public static final int OPC_SLTI = 10;

    /** Opcode for set less than immediate unsigned instruction */
    public static final int OPC_SLTIU = 11;
    
    /** Opcode for and immediate instruction */
    public static final int OPC_ANDI = 12;
    
    /** Opcode for or immediate instruction */
    public static final int OPC_ORI = 13;
    
    /** Opcode for xor immediate instruction */
    public static final int OPC_XORI = 14;    

    /** Opcode for load upper immediate instruction */
    public static final int OP_LUI = 15;   
    
    /** Opcode for Translation Lookaside Buffer */
    public static final int OP_TLB = 16;       
    
    /** Opcode for floating point unit instruction */
    public static final int OP_FPU = 17;   
    
    /** Opcode for coprocessor 2 instruction */
    public static final int OP_COPR2 = 18;       
    
    /** Opcode for coprocessor 3 instruction */
    public static final int OP_COPR3 = 19;       
    
    /** Opcode for load byte instruction */
    public static final int OP_LB = 32;      
    
    /** Opcode for load halfword instruction */   
    public static final int OP_LH = 33;     
            
    /** Opcode for load word left instruction */
    public static final int OP_LWL = 34;
    
    /** Opcode for load word instruction */
    public static final int OP_LW = 35;
            
    /** Opcode for load byte unsigned instruction */
    public static final int OP_LBU = 36;
    
    /** Opcode for load halfword unsigned instruction */   
    public static final int OP_LHU = 37;     
                
    /** Opcode for load word right instruction */
    public static final int OP_LWR = 38;    
    
    /** Opcode for store byte instruction */
    public static final int OP_SB = 40;     
    
    /** Opcode for store halfword instruction */
    public static final int OP_SH = 41;      
    
    /** Opcode for store word left instruction */
    public static final int OP_SWL = 42;  

    /** Opcode for store word instruction */
    public static final int OP_SW = 43;  

    /** Opcode for store word right instruction */
    public static final int OP_SWR = 46; 
    
    /** Opcode for load linker instruction */
    public static final int OP_LL = 48;     
    
    /** Opcode for load word coprocessor 1 instruction */
    public static final int OP_LWC1 = 49;    
    
     /** Opcode for store linker instruction */
    public static final int OP_SL = 56;
   
    /** Opcode for store word coprocessor 1 instruction */
    public static final int OP_SWC1 = 57;      
    
    
    // Branch for RT (destination register)
    
    /** Opcode for branch if less than zero instruction (destination register) */
    public static final int RT_BRANCH_BLTZ = 0;   
    
    /** Opcode for branch if greater of equal than zero instruction (destination register) */
    public static final int RT_BRANCH_BGEZ = 1;      
    
    /** Opcode for branch if less than zero and link instruction (destination register) */
    public static final int RT_BRANCH_BLTZAL = 16;   
    
    /** Opcode for branch if greater of equal than zero and link instruction (destination register) */
    public static final int RT_BRANCH_BGEZAL = 17;  
    
    
    
    /** Opcode for floating point unit move from coprocessor instruction (destination register) */
    public static final int RT_FPU_MFC1 = 0;   
    
    /** Opcode for floating point unit copy from coprocessor instruction (destination register) */
    public static final int RT_FPU_CFC1 = 2;     
    
    /** Opcode for floating point unit move to coprocessor instruction (destination register) */
    public static final int RT_FPU_MTC1 = 4;      
    
    /** Opcode for floating point unit copy to coprocessor instruction (destination register) */
    public static final int RT_FPU_CTC1 = 6;     
    
    /** Opcode for floating point unit branch to coprocessor instruction (destination register) */
    public static final int RT_FPU_BC1 = 8;
    
    /** Opcode for floating point unit single instruction (destination register) */
    public static final int RT_FPU_SINGLE = 16;    
    
    
    
    // R-Type subcode instructions:
    
    /** Opcode for shift left logical r-type instruction (subcode) */
    public static final int SUB_R_TYP_SLL = 0;
    
    /** Opcode for shift right logical r-type instruction (subcode) */
    public static final int SUB_R_TYP_SRL = 2;
    
    /** Opcode for shift right aritmetic r-type instruction (subcode) */
    public static final int SUB_R_TYP_SRA = 3;    
    
    /** Opcode for shift left logic variable r-type instruction (subcode) */
    public static final int SUB_R_TYP_SLLV = 4;    
    
    /** Opcode for shift right logic variable r-type instruction (subcode) */
    public static final int SUB_R_TYP_SRLV = 6;     
    
    /** Opcode for shift right aritmetic variable r-type instruction (subcode) */
    public static final int SUB_R_TYP_SRAV = 7;     
    
    /** Opcode for shift jump register r-type instruction (subcode) */
    public static final int SUB_R_TYP_JR = 8;         
           
    /** Opcode for jump and link register r-type instruction (subcode) */
    public static final int SUB_R_TYP_JALR = 9;
    
    /** Opcode for system call r-type instruction (subcode) */
    public static final int SUB_R_TYP_SYSCALL = 12; 
    
    /** Opcode for break instruction (subcode) */
    public static final int SUB_R_TYP_BREAK = 13;     
    
    /** Opcode for move from hi instruction (subcode) */
    public static final int SUB_R_TYP_MFHI = 16;     
    
    /** Opcode for move to hi instruction (subcode) */
    public static final int SUB_R_TYP_MTHI = 17;     
    
    /** Opcode for move from lo instruction (subcode) */
    public static final int SUB_R_TYP_MFLO = 18;       
    
    /** Opcode for move to lo instruction (subcode) */
    public static final int SUB_R_TYP_MTLO = 19;    
    
    /** Opcode for multiply instruction (subcode) */
    public static final int SUB_R_TYP_MULT = 24; 
    
    /** Opcode for multiply unsigned instruction (subcode) */
    public static final int SUB_R_TYP_MULTU = 25; 
    
    /** Opcode for division instruction (subcode) */
    public static final int SUB_R_TYP_DIV = 26; 
    
    /** Opcode for division unsigned instruction (subcode) */
    public static final int SUB_R_TYP_DIVU = 27;     
    
    /** Opcode for addition instruction (subcode) */
    public static final int SUB_R_TYP_ADD = 32; 
    
    /** Opcode for addition unsigned instruction (subcode) */
    public static final int SUB_R_TYP_ADDU = 33;     
    
    /** Opcode for subtraction instruction (subcode) */
    public static final int SUB_R_TYP_SUB = 34; 
    
    /** Opcode for subtraction unsigned instruction (subcode) */
    public static final int SUB_R_TYP_SUBU = 35;         
    
    /** Opcode for and instruction (subcode) */
    public static final int SUB_R_TYP_AND = 36;      
    
    /** Opcode for or instruction (subcode) */
    public static final int SUB_R_TYP_OR = 37;
    
    /** Opcode for xor instruction (subcode) */
    public static final int SUB_R_TYP_XOR = 38;      
    
    /** Opcode for nor instruction (subcode) */
    public static final int SUB_R_TYP_NOR = 39;      
    
    /** Opcode for set on less then instruction (subcode) */
    public static final int SUB_R_TYP_SLT = 42;     
    
    /** Opcode for set on less unsigned then instruction (subcode) */
    public static final int SUB_R_TYP_SLTU = 43;    
}
