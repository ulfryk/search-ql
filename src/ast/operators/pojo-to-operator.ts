import { OperatorType } from '../../common/model';
import { IOperator } from '../../dto';
import { AndOperator, GteOperator, GtOperator, IsNotOperator, IsOperator, LikeOperator, LteOperator, LtOperator, NotLikeOperator, OrOperator } from './binary';

// tslint:disable-next-line:cyclomatic-complexity
export const pojoToOperator = (pojo: IOperator) => {
  switch (pojo.type) {
    case OperatorType.And: return new AndOperator(pojo.token);
    case OperatorType.Or: return new OrOperator(pojo.token);
    case OperatorType.Like: return new LikeOperator(pojo.token);
    case OperatorType.NotLike: return new NotLikeOperator(pojo.token);
    case OperatorType.Is: return new IsOperator(pojo.token);
    case OperatorType.IsNot: return new IsNotOperator(pojo.token);
    case OperatorType.Gt: return new GtOperator(pojo.token);
    case OperatorType.Gte: return new GteOperator(pojo.token);
    case OperatorType.Lt: return new LtOperator(pojo.token);
    case OperatorType.Lte: return new LteOperator(pojo.token);
    default: throw Error(`No such operator type: "${pojo.type}"`);
  }
};
