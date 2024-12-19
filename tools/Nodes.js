const nodes = {
    Produccion: ['id', 'exprs', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['expr', 'label', 'qty'],
    Literal: ['val', 'isCase']
};

export default nodes;