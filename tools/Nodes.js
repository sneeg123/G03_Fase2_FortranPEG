const nodes = {
  Produccion: ["id", "exprs", "alias"],
  Opciones: ["exprs"],
  Union: ["exprs"],
  Expresion: ["expr", "id", "qty"],
  Literal: ["val", "isCase"],
};

export default nodes;
