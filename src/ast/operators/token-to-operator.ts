// tslint:disable-next-line:no-import-side-effect
import '@samwise-tech/immutable/Iterable/getMaybe';

import { Map } from 'immutable';

import { OperatorType, SyntaxConfig } from '../../config';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from './binary';
import { Operator } from './operator';
import { NotOperator } from './unary';

const operatorMapping = Map<OperatorType, (token: string) => BinaryOperator>([
  [OperatorType.And, (token: string) => new AndOperator(token)],
  [OperatorType.Like, (token: string) => new LikeOperator(token)],
  [OperatorType.Not, (token: string) => new NotOperator(token)],
  [OperatorType.Or, (token: string) => new OrOperator(token)],
]);

export const tokenToOperator = (config: SyntaxConfig) => (token: string): Operator =>
  operatorMapping.getMaybe(config.getOperatorType(token)).cata(() => {
    throw Error(`Unknown OperatorType for token: "${token}"`);
  }, getOperator => getOperator(token));
