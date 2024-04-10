// Copyright 2000-2005 the Contributors, as shown in the revision logs.
// Licensed under the Apache License 2.0 ("the License").
// You may not use this file except in compliance with the License.

package org.ibex.nestedvm.util;

import java.io.*;

/**
 * Manage ELF binary file format
 */
public class ELF {
    /** Elf Magic header */
    private static final int ELF_MAGIC = 0x7f454c46; // '\177', 'E', 'L', 'F'

    /** Elf class none */
    public static final int ELFCLASSNONE = 0;
    
    /** Elf class 32 bit */
    public static final int ELFCLASS32 = 1;
    
    /** Elf class 64 bit */
    public static final int ELFCLASS64 = 2;

    public static final int ELFDATANONE = 0;
    
    /** Elf data in little endiam mode */
    public static final int ELFDATA2LSB = 1;
    
    /** Elf data in gig endian mode */
    public static final int ELFDATA2MSB = 2;

    /** Section of symbol table */
    public static final int SHT_SYMTAB = 2;
    
    /** Section of string table  */
    public static final int SHT_STRTAB = 3;
    
    /** Section of space used at runtime */
    public static final int SHT_NOBITS = 8;

    /** Section of data that can be written during the execution */
    public static final int SHF_WRITE = 1;
    
    /** Section of data that are allocated during the exection */
    public static final int SHF_ALLOC = 2;
    
    /** Section of executable code */
    public static final int SHF_EXECINSTR = 4;

    /** Executabler segment */
    public static final int PF_X = 0x1;
    
    /** Writeable segment */
    public static final int PF_W = 0x2;
    
    /** Loadable segment */
    public static final int PF_R = 0x4;

    /** Program type: load */
    public static final int PT_LOAD = 1;

    /** Executable type: executable */
    public static final short ET_EXEC = 2;
    
    /** Executable machine: Mips*/
    public static final short EM_MIPS = 8;


    private Seekable data;

    /** ELF ident */
    public ELFIdent ident;
    
    /** Elf header */
    public ELFHeader header;
    
    /** Program headers */
    public PHeader[] pheaders;
    
    /** Section headers */
    public SHeader[] sheaders;

    private byte[] stringTable;
    private boolean sectionReaderActive;

    /**
     * Read fully the given buffer
     * 
     * @param buf the buffer to fill with read data
     * @throws IOException Exception in case of end of file
     */
    private void readFully(byte[] buf) throws IOException {
        int len = buf.length;
        int pos = 0;
        while(len > 0) {
            int n = data.read(buf,pos,len);
            if(n == -1) throw new IOException("EOF");
            pos += n;
            len -= n;
        }
    }

    /**
     * Read an int in big endian format 
     * 
     * @return the read int
     * @throws IOException Exception in case of end of file
     */
    private int readIntBE() throws IOException {
        byte[] buf = new byte[4];
        readFully(buf);
        return ((buf[0]&0xff)<<24)|((buf[1]&0xff)<<16)|((buf[2]&0xff)<<8)|(buf[3]&0xff);
    }
    
    /**
     * Read an int in little endian format 
     * 
     * @return the read int
     * @throws IOException Exception in case of end of file
     */
    private int readInt() throws IOException {
        int x = readIntBE();
        if(ident!=null && ident.data == ELFDATA2LSB)
            x = ((x<<24)&0xff000000) | ((x<<8)&0xff0000) | ((x>>>8)&0xff00) | ((x>>24)&0xff);
        return x;
    }

    /**
     * Read a short in big endian format 
     * 
     * @return the read short
     * @throws IOException Exception in case of end of file
     */
    private short readShortBE() throws IOException {
        byte[] buf = new byte[2];
        readFully(buf);
        return (short)(((buf[0]&0xff)<<8)|(buf[1]&0xff));
    }
    
    /**
     * Read a short in little endian format
     * 
     * @return the read short
     * @throws IOException Exception in case of end of file
     */
    private short readShort() throws IOException {
        short x = readShortBE();
        if(ident!=null && ident.data == ELFDATA2LSB)
            x = (short)((((x<<8)&0xff00) | ((x>>8)&0xff))&0xffff);
        return x;
    }

    /**
     * Read a byte 
     * @return the read byte
     * @throws IOException Exception in case of end of file
     */
    private byte readByte() throws IOException {
        byte[] buf = new byte[1];
        readFully(buf);
        return buf[0];
    }

    /**
     * Ident of ELF binary file
     */
    public class ELFIdent {
        /** Class (32/64) of ELF */
        public byte klass;
        
        /** Endian type of data */
        public byte data;
        
        /** ABI of the operatin system */
        public byte osabi;
        
        /** Version of the ABI */
        public byte abiversion;

        /**
         * Construct the Ident of ELF
         * 
         * @throws IOException 
         */
        ELFIdent() throws IOException {
            if(readIntBE() != ELF_MAGIC) throw new ELFException("Bad Magic");

            klass = readByte();
            if(klass != ELFCLASS32) throw new ELFException("org.ibex.nestedvm.util.ELF does not suport 64-bit binaries");

            data = readByte();
            if(data != ELFDATA2LSB && data != ELFDATA2MSB) throw new ELFException("Unknown byte order");

            readByte(); // version
            osabi = readByte();
            abiversion = readByte();
            for(int i=0; i<7; i++) readByte(); // padding
        }
    }

    /**
     * Header of ELF binary file
     */
    public class ELFHeader {
        /** Type of ELF */
        public short type;
        
        /** Machine architecture */
        public short machine;
        
        /** ELF version */
        public int version;
        
        /** Entry point virtual address */
        public int entry;
        
        /** Offset of program header in memory */
        public int phoff;
        
        /** Offset of section heade in memory */
        public int shoff;
        
        /** Machine flag */
        public int flags;
        
        /** Header dimension */
        public short ehsize;
        
        /** Program header dimension */
        public short phentsize;
        
        /** Number of entry of program header */
        public short phnum;
        
        /** Section header dimension */
        public short shentsize;
        
        /** Number of entry of section header */
        public short shnum;
        
        /** Section header index for section name */
        public short shstrndx;

         /**
         * Construct the Header of ELF
         * 
         * @throws IOException 
         */
        ELFHeader() throws IOException {
            type = readShort();
            machine = readShort();
            version = readInt();
            if(version != 1) throw new ELFException("version != 1");
            entry = readInt();
            phoff = readInt();
            shoff = readInt();
            flags = readInt();
            ehsize = readShort();
            phentsize = readShort();
            phnum = readShort();
            shentsize = readShort();
            shnum = readShort();
            shstrndx = readShort();
        }
    }

    /**
     * Program header of ELF
     */
    public class PHeader {
        /** Type of segment */
        public int type;
        
        /** Offset of segment in the file */
        public int offset;
        
        /** Virtual address of memory segment */
        public int vaddr;
        
        /** Physical address of memory segment */                
        public int paddr;
        
        /** Dimension of file segment*/
        public int filesz;
        
        /** Dimension of memory segment */
        public int memsz;
        
        /** Segment flag */
        public int flags;
        
        /** Aligment of segment */
        public int align;

        /**
         * Construct the Program Header of ELF
         * 
         * @throws IOException 
         */
        PHeader() throws IOException {
            type = readInt();
            offset = readInt();
            vaddr = readInt();
            paddr = readInt();
            filesz = readInt();
            memsz = readInt();
            flags = readInt();
            align = readInt();
            if(filesz > memsz) throw new ELFException("ELF inconsistency: filesz > memsz (" + toHex(filesz) + " > " + toHex(memsz) + ")");
        }

        /**
         * Return true if the program header is writeable
         * 
         * @return true if writeable 
         */
        public boolean writable() { return (flags & PF_W) != 0; }

        /**
         * Get the input stream for this section 
         * 
         * @return the input stream
         * @throws IOException  Exception in case of error
         */
        public InputStream getInputStream() throws IOException {
            return new BufferedInputStream(new SectionInputStream(
                offset,offset+filesz));
        }
    }

    /**
     * Section header of ELF
     */
    public class SHeader {
        /** Name index */
        int nameidx;
        
        /** Section name */
        public String name;
        
        /** Section type */
        public int type;
        
        /** Section flag */
        public int flags;
        
        /** Section address */
        public int addr;
        
        /** Section offset */
        public int offset;
        
        /** Section dimension */
        public int size;
        
        /** Section link */
        public int link;
        
        /** Section additional information */
        public int info;
        
        /** Section aligment */
        public int addralign;
        
        /** Entry section dimension */
        public int entsize;

        /**
         * Construct the Section Header of ELF
         * 
         * @throws IOException 
         */
        SHeader() throws IOException {
            nameidx = readInt();
            type = readInt();
            flags = readInt();
            addr = readInt();
            offset = readInt();
            size = readInt();
            link = readInt();
            info = readInt();
            addralign = readInt();
            entsize = readInt();
        }

        /**
         * Get the input stream for this section 
         * 
         * @return the input stream
         * @throws IOException  Exception in case of error
         */
        public InputStream getInputStream() throws IOException {
            return new BufferedInputStream(new SectionInputStream(
                offset, type == SHT_NOBITS ? 0 : offset+size));
        }

        /**
         * Return if section is of text
         * 
         * @return true if section of text
         */
        public boolean isText() { return name.equals(".text"); }
        
        /**
         * Return if section is of data
         * 
         * @return true id section of data
         */
        public boolean isData() { return name.equals(".data") || name.equals(".sdata") || name.equals(".rodata") || name.equals(".ctors") || name.equals(".dtors"); }
        
        /**
         * Return if section is of block started by symbol
         * 
         * @return true if section is of bss
         */        
        public boolean isBSS() { return name.equals(".bss") || name.equals(".sbss"); }
    }

    /**
     * Create the ELF from the given file
     * 
     * @param file the ELF file
     * @throws IOException Generic IO exception
     * @throws org.ibex.nestedvm.util.ELF.ELFException ELF specific exception
     */
    public ELF(String file) throws IOException, ELFException { 
      this(new Seekable.File(file,false)); 
    }
    
    /**
     * Create the ELF from the given data
     * 
     * @param data the ELF data
     * @throws IOException Generic IO exception
     * @throws org.ibex.nestedvm.util.ELF.ELFException  ELF specific exception
     */
    public ELF(Seekable data) throws IOException, ELFException {
        this.data = data;
        ident = new ELFIdent();
        header = new ELFHeader();
        pheaders = new PHeader[header.phnum];
        for(int i=0;i<header.phnum;i++) {
            data.seek(header.phoff+i*header.phentsize);
            pheaders[i] = new PHeader();
        }
        sheaders = new SHeader[header.shnum];
        for(int i=0;i<header.shnum;i++) {
            data.seek(header.shoff+i*header.shentsize);
            sheaders[i] = new SHeader();
        }
        if(header.shstrndx < 0 || header.shstrndx >= header.shnum) throw new ELFException("Bad shstrndx");
        data.seek(sheaders[header.shstrndx].offset);
        stringTable = new byte[sheaders[header.shstrndx].size];
        readFully(stringTable);

        for(int i=0;i<header.shnum;i++) {
            SHeader s = sheaders[i];
            s.name = getString(s.nameidx);
        }
    }

    /**
     * Get a string from the specific offset
     * 
     * @param off the string offset
     * @return the string
     */
    private String getString(int off) { 
        return getString(off, stringTable); 
    }
    
    /**
     * Get a string from the specific offset from a buffer
     * @param off the string offset 
     * @param strtab the buffer
     * @return the string 
     */
    private String getString(int off, byte[] strtab) {
        StringBuffer sb = new StringBuffer();
        if(off < 0 || off >= strtab.length) return "<invalid strtab entry>";
        while(off >= 0 && off < strtab.length && strtab[off] != 0) sb.append((char)strtab[off++]);
        return sb.toString();
    }

    /**
     * Get the section header with the given name
     * 
     * @param name the name of section
     * @return the section header
     */
    public SHeader sectionWithName(String name) {
        for (SHeader sheader : sheaders) {
            if (sheader.name.equals(name)) {
                return sheader;
            }
        }
        return null;
    }

    /**
     * ELF custom exception
     */
    @SuppressWarnings("serial")
    public class ELFException extends IOException {
      ELFException(String s) { 
          super(s);
      } 
    }

    /**
     * Input stream for read an ELF section
     */
    private class SectionInputStream extends InputStream {
        /** Current position */
        private int pos;
        
        /** Max position */
        private int maxpos;
        
        /**
         * Construct the section input stream from the given position
         * 
         * @param start starting position
         * @param end rnding position
         * @throws IOException IO exception in case of error
         */
        SectionInputStream(int start, int end) throws IOException {
            if(sectionReaderActive)
                throw new IOException("Section reader already active");
            sectionReaderActive = true;
            pos = start;
            data.seek(pos);
            maxpos = end;
        }

        /**
         * Byte left to read in the stream 
         * 
         * @return the byte left to read
         */
        private int bytesLeft() { 
            return maxpos - pos;
        }
        
        /**
         * Read and int from stream
         * 
         * @return the int readed from stream
         * @throws IOException IO exception in case of error
         */
        @Override
        public int read() throws IOException {
            byte[] buf = new byte[1];
            return read(buf,0,1) == -1 ? -1 : (buf[0]&0xff);
        }
        
        /**
         * Read and int from stream in given buffer
         * 
         * @param b the buffer
         * @param off the starting offset position in buffer
         * @param len the lenght to read
         * @return the int read from stream
         * @throws IOException IO exception in case of error
         */
        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            int n = data.read(b,off,Math.min(len,bytesLeft())); if(n > 0) pos += n; return n;
        }
        
        /**
         * Close the stream
         */
        @Override
        public void close() { 
            sectionReaderActive = false; 
        }
    }

    private Symtab _symtab;
    
    /**
     * Get the symbol table
     * 
     * @return the symbol table
     * @throws IOException IO Exception in case of error
     */
    public Symtab getSymtab() throws IOException {
        if(_symtab != null) return _symtab;

        if(sectionReaderActive) throw new ELFException("Can't read the symtab while a section reader is active");

        SHeader sh = sectionWithName(".symtab");
        if(sh == null || sh.type != SHT_SYMTAB) return null;

        SHeader sth = sectionWithName(".strtab");
        if(sth == null || sth.type != SHT_STRTAB) return null;

        byte[] strtab = new byte[sth.size];
        DataInputStream dis = new DataInputStream(sth.getInputStream());
        dis.readFully(strtab);
        dis.close();

        return _symtab = new Symtab(sh.offset, sh.size, strtab);
    }

    /**
     * Symbol table
     */
    public class  Symtab {
        public Symbol[] symbols;

        /**
         * Construct the symbol table from the given buffer and position
         * 
         * @param off starting offset in buffer
         * @param size section dimension
         * @param strtab buffer
         * @throws IOException IO Exception in case of error
         */
        Symtab(int off, int size, byte[] strtab) throws IOException {
            data.seek(off);
            int count = size/16;
            symbols = new Symbol[count];
            for(int i=0; i<count; i++) symbols[i] = new Symbol(strtab);
        }

        /**
         * Get the symbol table with the given name
         * 
         * @param name the name of symbol
         * @return the symbol
         */
        public Symbol getSymbol(String name) {
            Symbol sym = null;
            for (Symbol symbol : symbols) {
                if (symbol.name.equals(name)) {
                    if (sym == null) {
                        sym = symbol;
                    } else {
                        System.err.println("WARNING: Multiple symbol matches for " + name);
                    }
                }
            }
            return sym;
        }

        /**
         * Get the global symbol with the given name
         * 
         * @param name the name of global symbol
         * @return the symbol
         */
        public Symbol getGlobalSymbol(String name) {
            for (Symbol symbol : symbols) {
                if (symbol.binding == Symbol.STB_GLOBAL && symbol.name.equals(name)) {
                    return symbol;
                }
            }
            return null;
        }
    }

    /**
     * Symbol inside ELF
     */
    public class Symbol {
        /** Symbol name */
        public String name;
        
        /** Symbol address */
        public int addr;
        
        /** Symbol size */
        public int size;
        
        /** Symbol additional information */
        public byte info;
        
        /** Symbol type */
        public byte type;
        
        /** Symbol binding */
        public byte binding;
        
        /** Other symbol attribute */
        public byte other;
        
        /** Index of symbol section */
        public short shndx;
        
        /** Section header */
        public SHeader sheader;

        /** Symbol as function  */
        public final static int STT_FUNC = 2;
        
        /** Symbol ad global */
        public final static int STB_GLOBAL = 1;

        /**
         * Construct the symbol with the given buffer
         * 
         * @param strtab the buffer
         * @throws IOException IO Exception in case of error
         */
        Symbol(byte[] strtab) throws IOException {
            name = getString(readInt(), strtab);
            addr = readInt();
            size = readInt();
            info = readByte();
            type = (byte)(info&0xf);
            binding = (byte)(info>>4);
            other = readByte();
            shndx = readShort();
        }
    }

    /**
     * Convert an int number to hex representation
     * 
     * @param n the number to convert
     * @return the hex representation
     */
    private static String toHex(int n) { 
        return "0x" + Long.toString(n & 0xffffffffL, 16); 
    }
}
