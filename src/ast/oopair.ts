import { Maybe, None, Some } from 'monet';

import { Expression } from '../common/model';

// TODO: Consider creating class for this…
type OOPair<E extends Expression = Expression> = [Maybe<string>, E];

const initialPair = (leftOperand: Expression): OOPair => [None<string>(), leftOperand];
const restPair = (operator: string, operand: Expression): OOPair => [Some(operator), operand];

export { initialPair, OOPair, restPair };
