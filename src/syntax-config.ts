export const AND = 'AND';
export const NOT = 'NOT';
export const OR = 'OR';
export const GROUP_START = '(';
export const GROUP_END = ')';
export const EXACT_MATCHER = '"';
export const LABEL_DELIMITER = ':';

export type LogicOperator = typeof AND | typeof NOT | typeof OR;
