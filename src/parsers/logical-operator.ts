import * as P from 'parsimmon';

import { SyntaxConfig } from '../config';

export const logicalOperator = ({ AND, NOT, OR }: SyntaxConfig) =>
  P.alt(P.string(AND), P.string(NOT), P.string(OR));
