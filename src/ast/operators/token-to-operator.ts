import { Map } from 'immutable';
import { Maybe } from 'monet';

import { OperatorType } from '../../common/model';
import { ParserConfig } from '../../config';

import { AndOperator, BinaryOperator, GteOperator, GtOperator, IsNotOperator, IsOperator, LikeOperator, LteOperator, LtOperator, NotLikeOperator, OrOperator } from './binary';
import { Operator } from './operator';
import { NotOperator } from './unary';

const operatorMapping = Map<OperatorType, (token: string) => BinaryOperator>([
  [OperatorType.And, (token: string) => new AndOperator(token)],
  [OperatorType.Is, (token: string) => new IsOperator(token)],
  [OperatorType.IsNot, (token: string) => new IsNotOperator(token)],
  [OperatorType.Like, (token: string) => new LikeOperator(token)],
  [OperatorType.NotLike, (token: string) => new NotLikeOperator(token)],
  [OperatorType.Not, (token: string) => new NotOperator(token)],
  [OperatorType.Or, (token: string) => new OrOperator(token)],
  [OperatorType.Gt, (token: string) => new GtOperator(token)],
  [OperatorType.Gte, (token: string) => new GteOperator(token)],
  [OperatorType.Lt, (token: string) => new LtOperator(token)],
  [OperatorType.Lte, (token: string) => new LteOperator(token)],
]);

export const tokenToOperator = (config: ParserConfig) => (token: string): Operator =>
  Maybe.fromNull(operatorMapping.get(config.getOperatorType(token))).cata(() => {
    throw Error(`Unknown OperatorType for token: "${token}"`);
  }, getOperator => getOperator(token));
