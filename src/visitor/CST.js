
// Auto-generated
import Node from './Node.js';

export class Produccion extends Node {
    constructor(id, exprs, alias) {
        super();
        this.id = id;
		this.exprs = exprs;
		this.alias = alias;
    }

    accept(visitor) {
        return visitor.visitProduccion(this);
    }
}
    
export class Opciones extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitOpciones(this);
    }
}
    
export class Union extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitUnion(this);
    }
}
    
export class Expresion extends Node {
    constructor(expr, label, qty) {
        super();
        this.expr = expr;
		this.label = label;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitExpresion(this);
    }
}
    
export class Literal extends Node {
    constructor(val, isCase) {
        super();
        this.val = val;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitLiteral(this);
    }
}
    