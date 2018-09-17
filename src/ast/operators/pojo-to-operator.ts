import { OperatorType } from '../../common/model';
import { IOperator } from '../../dto';
import { AndOperator, IsNotOperator, IsOperator, LikeOperator, NotLikeOperator, OrOperator } from './binary';

// tslint:disable-next-line:cyclomatic-complexity
export const pojoToOperator = (pojo: IOperator) => {
  switch (pojo.type) {
    case OperatorType.And: return new AndOperator(pojo.token);
    case OperatorType.Or: return new OrOperator(pojo.token);
    case OperatorType.Like: return new LikeOperator(pojo.token);
    case OperatorType.NotLike: return new NotLikeOperator(pojo.token);
    case OperatorType.Is: return new IsOperator(pojo.token);
    case OperatorType.IsNot: return new IsNotOperator(pojo.token);
    default: throw Error(`No such operator type: "${pojo.type}"`);
  }
};
