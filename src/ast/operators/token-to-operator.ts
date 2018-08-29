import { Map } from 'immutable';
import { Maybe } from 'monet';

import { OperatorType } from '../../common/model';
import { SyntaxConfig } from '../../config';

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
  Maybe.fromNull(operatorMapping.get(config.getOperatorType(token))).cata(() => {
    throw Error(`Unknown OperatorType for token: "${token}"`);
  }, getOperator => getOperator(token));
