export default function fasm(hljs) {
  const IDENT_RE = '[a-zA-Z_?.@][\\w?.@]*';

  const KEYWORDS = {
    keyword: [
      // Data definition
      'db', 'dw', 'dd', 'dp', 'dq', 'dt', 'do', 'dy', 'dz',
      'du', 'rb', 'rw', 'rd', 'rp', 'rq', 'rt', 'ro', 'ry', 'rz',

      // Assembly directives
      'format', 'use', 'section', 'segment',
      'public', 'extrn', 'common', 'entry',
      'stack', 'heap', 'resource', 'import',
      'export', 'include', 'file', 'virtual',
      'load', 'store', 'display', 'assert',
      'equ', 'fix', 'define', 'purge',
      'macro', 'struc', 'struct', 'union',
      'ends', 'end', 'repeat', 'rept',
      'while', 'if', 'else', 'elseif',
      'ifdefined', 'ifused', 'ifnotused',
      'match', 'irp', 'irps', 'irpv',
      'restore', 'local', 'forward',
      'reverse', 'common', 'extrn',

      // Data and memory directives
      'align', 'virtual', 'load', 'store',
      'rb', 'rw', 'rd', 'rq', 'rt',
      'du', 'dp', 'dq', 'dt',

      // FASM operators
      'and', 'or', 'xor', 'not',
      'shl', 'shr', 'sal', 'sar',
      'mod', 'rem', 'ror', 'rol',
      'eq', 'ne', 'lt', 'gt',
      'le', 'ge', 'in', 'eqtype',
      'eqstr', 'contains', 'isin',

      // CPU directives
      'use16', 'use32', 'use64',
      'processor', '8086', '186',
      '286', '386', '486', '586',
      '686', 'p2', 'p3', 'p4'
    ],

    literal: [
      'true', 'false', 'nil'
    ]
  };

  const REGISTERS = [
    // General purpose registers
    'rax', 'rbx', 'rcx', 'rdx',
    'rsi', 'rdi', 'rbp', 'rsp',
    'eax', 'ebx', 'ecx', 'edx',
    'esi', 'edi', 'ebp', 'esp',
    'ax', 'bx', 'cx', 'dx',
    'si', 'di', 'bp', 'sp',
    'al', 'ah', 'bl', 'bh',
    'cl', 'ch', 'dl', 'dh',

    // Extended registers
    ...Array.from({ length: 16 }, (_, i) => `r${i}`),
    ...Array.from({ length: 16 }, (_, i) => `r${i}d`),
    ...Array.from({ length: 16 }, (_, i) => `r${i}w`),
    ...Array.from({ length: 16 }, (_, i) => `r${i}b`),

    // Segment registers
    'cs', 'ds', 'es', 'fs', 'gs', 'ss',

    // Control and debug registers
    ...Array.from({ length: 9 }, (_, i) => `cr${i}`),
    ...Array.from({ length: 9 }, (_, i) => `dr${i}`),

    // x87
    ...Array.from({ length: 8 }, (_, i) => `st${i}`),

    // MMX
    ...Array.from({ length: 8 }, (_, i) => `mm${i}`),

    // SIMD
    ...Array.from({ length: 32 }, (_, i) => `xmm${i}`),
    ...Array.from({ length: 32 }, (_, i) => `ymm${i}`),
    ...Array.from({ length: 32 }, (_, i) => `zmm${i}`),

    // Mask registers
    ...Array.from({ length: 8 }, (_, i) => `k${i}`),

    // Bound registers
    'bnd0', 'bnd1', 'bnd2', 'bnd3',

    // Instruction pointer and flags
    'rip', 'eip', 'ip',
    'rflags', 'eflags', 'flags',

    // x87 control registers
    'st'
  ];

  const INSTRUCTIONS = [
    // Data movement
    'mov', 'movzx', 'movsx', 'movsxd',
    'movbe', 'movsb', 'movsw', 'movsd',
    'movsq', 'movd', 'movq', 'movaps',
    'movups', 'movapd', 'movupd',
    'movdqa', 'movdqu', 'movss', 'movsd',
    'lea', 'xchg', 'bswap', 'xlat',

    // Stack
    'push', 'pop', 'pusha', 'pushad',
    'popa', 'popad', 'pushf', 'pushfd',
    'pushfq', 'popf', 'popfd', 'popfq',
    'enter', 'leave',

    // Arithmetic
    'add', 'adc', 'sub', 'sbb',
    'mul', 'imul', 'div', 'idiv',
    'inc', 'dec', 'neg', 'cmp',
    'xadd', 'cmpxchg', 'cmpxchg8b',
    'cmpxchg16b',

    // Logical
    'and', 'or', 'xor', 'not',
    'test', 'bt', 'bts', 'btr', 'btc',
    'bsf', 'bsr', 'tzcnt', 'lzcnt',
    'popcnt',

    // Shifts and rotates
    'shl', 'shr', 'sal', 'sar',
    'rol', 'ror', 'rcl', 'rcr',
    'shld', 'shrd', 'shlx', 'shrx', 'sarx',

    // Control flow
    'jmp', 'call', 'ret', 'retn', 'retf',
    'loop', 'loope', 'loopne', 'loopz', 'loopnz',
    'jcxz', 'jecxz', 'jrcxz',

    // Conditional jumps
    'ja', 'jae', 'jb', 'jbe',
    'jc', 'je', 'jg', 'jge',
    'jl', 'jle', 'jna', 'jnae',
    'jnb', 'jnbe', 'jnc', 'jne',
    'jng', 'jnge', 'jnl', 'jnle',
    'jno', 'jnp', 'jns', 'jnz',
    'jo', 'jp', 'jpe', 'jpo',
    'js', 'jz',

    // Conditional moves
    'cmova', 'cmovae', 'cmovb', 'cmovbe',
    'cmovc', 'cmove', 'cmovg', 'cmovge',
    'cmovl', 'cmovle', 'cmovna', 'cmovnae',
    'cmovnb', 'cmovnbe', 'cmovnc', 'cmovne',
    'cmovng', 'cmovnge', 'cmovnl', 'cmovnle',
    'cmovno', 'cmovnp', 'cmovns', 'cmovnz',
    'cmovo', 'cmovp', 'cmovpe', 'cmovpo',
    'cmovs', 'cmovz',

    // Set condition
    'seta', 'setae', 'setb', 'setbe',
    'setc', 'sete', 'setg', 'setge',
    'setl', 'setle', 'setna', 'setnae',
    'setnb', 'setnbe', 'setnc', 'setne',
    'setng', 'setnge', 'setnl', 'setnle',
    'setno', 'setnp', 'setns', 'setnz',
    'seto', 'setp', 'setpe', 'setpo',
    'sets', 'setz',

    // String operations
    'movs', 'movsb', 'movsw', 'movsd', 'movsq',
    'stos', 'stosb', 'stosw', 'stosd', 'stosq',
    'lods', 'lodsb', 'lodsw', 'lodsd', 'lodsq',
    'scas', 'scasb', 'scasw', 'scasd', 'scasq',
    'cmps', 'cmpsb', 'cmpsw', 'cmpsd', 'cmpsq',
    'ins', 'insb', 'insw', 'insd',
    'outs', 'outsb', 'outsw', 'outsd',

    // Flags
    'clc', 'stc', 'cmc', 'cld', 'std',
    'cli', 'sti', 'lahf', 'sahf',
    'pushf', 'popf',

    // Interrupts and system
    'int', 'int3', 'into', 'iret', 'iretd', 'iretq',
    'syscall', 'sysret', 'sysenter', 'sysexit',
    'hlt', 'nop', 'ud2', 'cpuid',
    'rdtsc', 'rdtscp', 'rdmsr', 'wrmsr',
    'rdpmc', 'invd', 'wbinvd',
    'invlpg', 'invlpgb', 'swapgs',

    // Port I/O
    'in', 'out',

    // Bit manipulation
    'shld', 'shrd', 'bt', 'bts', 'btr', 'btc',
    'bsf', 'bsr', 'bswap',

    // x87
    'fld', 'fst', 'fstp', 'fild', 'fist', 'fistp',
    'fadd', 'faddp', 'fiadd',
    'fsub', 'fsubp', 'fisub',
    'fmul', 'fmulp', 'fimul',
    'fdiv', 'fdivp', 'fidiv',
    'fcom', 'fcomp', 'fcompp',
    'fucom', 'fucomp', 'fucompp',
    'fchs', 'fabs', 'fsqrt',
    'fsin', 'fcos', 'fsincos', 'fptan',
    'fpatan', 'f2xm1', 'fyl2x', 'fyl2xp1',
    'frndint', 'fscale', 'fxtract',
    'finit', 'fninit', 'fclex', 'fnclex',
    'fldcw', 'fstcw', 'fnstcw',
    'fldenv', 'fstenv', 'fnstenv',
    'fsave', 'fnsave', 'frstor',
    'fwait', 'wait',

    // SSE / AVX and other SIMD
    'addps', 'addpd', 'addss', 'addsd',
    'subps', 'subpd', 'subss', 'subsd',
    'mulps', 'mulpd', 'mulss', 'mulsd',
    'divps', 'divpd', 'divss', 'divsd',
    'sqrtps', 'sqrtpd', 'sqrtss', 'sqrtsd',
    'minps', 'minpd', 'minss', 'minsd',
    'maxps', 'maxpd', 'maxss', 'maxsd',
    'andps', 'andpd', 'andnps', 'andnpd',
    'orps', 'orpd', 'xorps', 'xorpd',
    'shufps', 'shufpd', 'unpckhps', 'unpcklps',
    'unpckhpd', 'unpcklpd',
    'movhlps', 'movlhps', 'movhps', 'movlps',
    'movhpd', 'movlpd',
    'movmskps', 'movmskpd',
    'comiss', 'comisd', 'ucomiss', 'ucomisd',
    'cmpps', 'cmppd', 'cmpss', 'cmpsd',
    'cvtss2si', 'cvtsd2si', 'cvtsi2ss', 'cvtsi2sd',
    'cvttps2dq', 'cvtps2dq', 'cvtdq2ps',
    'emms', 'sfence', 'lfence', 'mfence',
    'pause', 'prefetchnta', 'prefetcht0',
    'prefetcht1', 'prefetcht2',

    // Miscellaneous
    'bound', 'arpl', 'lar', 'lsl',
    'verr', 'verw', 'smsw', 'lmsw',
    'sgdt', 'sidt', 'lgdt', 'lidt',
    'sldt', 'str', 'lldt', 'ltr',
    'clts', 'invd', 'wbinvd',
    'rsm', 'monitor', 'mwait',
    'xgetbv', 'xsetbv'
  ];

  const REGISTER_RE = new RegExp(
    '\\b(?:' + REGISTERS.join('|') + ')\\b',
    'i'
  );

  const INSTRUCTION_RE = new RegExp(
    '\\b(?:' + INSTRUCTIONS.join('|') + ')\\b',
    'i'
  );

  const NUMBER = {
    className: 'number',
    relevance: 0,
    variants: [
      // Hexadecimal: $FF, 0FFh, 0xFF
      { begin: '\\$[0-9a-f]+' },
      { begin: '\\b[0-9][0-9a-f]*h\\b', relevance: 0 },
      { begin: '\\b0x[0-9a-f]+\\b', relevance: 0 },

      // Binary: 101b
      { begin: '\\b[01]+b\\b', relevance: 0 },

      // Octal: 377o, 377q
      { begin: '\\b[0-7]+[oq]\\b', relevance: 0 },

      // Decimal
      { begin: '\\b\\d+\\b', relevance: 0 }
    ]
  };

  const STRING = {
    className: 'string',
    variants: [
      {
        begin: "'",
        end: "'",
        contains: [
          { begin: "''" }
        ]
      },
      {
        begin: '"',
        end: '"',
        contains: [
          { begin: '""' }
        ]
      }
    ]
  };

  const COMMENT = hljs.COMMENT(';', '$', {
    relevance: 0
  });

  const DIRECTIVES = [
    'format', 'section', 'segment', 'public',
    'extrn', 'common', 'entry', 'stack',
    'heap', 'resource', 'import', 'export',
    'include', 'file', 'virtual', 'load',
    'store', 'display', 'assert', 'align',
    'use16', 'use32', 'use64', 'processor'
  ];

  const ATTRIBUTES = [
    'executable', 'readable', 'writeable',
    'writable', 'code', 'data', 'resource',
    'discardable', 'shared', 'fixed',
    'dynamic', 'system', 'native',
    'console', 'gui', 'dll', 'driver'
  ];

  const FORMATS = [
    'ELF', 'ELF64', 'ELF32',
    'PE', 'PE64', 'PE32',
    'COFF', 'COFF64',
    'MZ', 'MSDOS', 'BINARY',
    'OBJ', 'LIB'
  ];

  return {
    name: 'Flat Assembler',
    aliases: ['fasm', 'flat-assembler'],
    case_insensitive: true,

    keywords: KEYWORDS,

    contains: [
      COMMENT,

      // Strings
      STRING,

      // Numeric constants
      NUMBER,

      // FASM address operators
      {
        className: 'built_in',
        begin: '\\${1,2}',
        relevance: 0
      },

      // Format identifiers
      {
        className: 'type',
        begin: '\\b(?:' + FORMATS.join('|') + ')\\b',
        relevance: 1
      },

      // Section attributes
      {
        className: 'attribute',
        begin: '\\b(?:' + ATTRIBUTES.join('|') + ')\\b',
        relevance: 0
      },

      // FASM directives
      {
        className: 'keyword',
        begin: '\\b(?:' + DIRECTIVES.join('|') + ')\\b',
        relevance: 1
      },

      // Registers
      {
        className: 'variable',
        begin: REGISTER_RE,
        relevance: 0
      },

      // Instructions
      {
        className: 'built_in',
        begin: INSTRUCTION_RE,
        relevance: 0
      },

      // Named labels
      {
        className: 'symbol',
        begin: '^[ \\t]*' + IDENT_RE + ':',
        end: '(?=\\s|$)',
        relevance: 0
      },

      // Anonymous labels: @b, @f
      {
        className: 'symbol',
        begin: '@[bf]\\b',
        relevance: 0
      },

      // Local labels: .label
      {
        className: 'symbol',
        begin: '\\.[a-zA-Z_][\\w]*',
        relevance: 0
      },

      // Size specifiers
      {
        className: 'type',
        begin: '\\b(?:byte|word|dword|pword|qword|tword|tbyte|oword|yword|zword)\\b',
        relevance: 0
      }
    ]
  };
}
