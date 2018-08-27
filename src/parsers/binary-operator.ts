import * as P from 'parsimmon';

import { SyntaxConfig } from '../config';

export const binaryOperator = ({ AND, NOT, OR }: SyntaxConfig) =>
  P.alt(...[AND, NOT, OR].map(operator => P.string(operator)));
