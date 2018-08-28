import * as P from 'parsimmon';

import { SyntaxConfig } from '../config';

// FIXME: NOT should move out maybe ?
export const unaryOperator = ({ unaryOperators }: SyntaxConfig) =>
  P.alt(...unaryOperators.map(operator => P.string(operator)));
