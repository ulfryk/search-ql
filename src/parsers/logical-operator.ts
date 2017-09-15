import * as P from 'parsimmon';

import { AND, NOT, OR } from '../syntax-config';

const logicalOperator = P.alt(P.string(AND), P.string(NOT), P.string(OR));

export { logicalOperator };
