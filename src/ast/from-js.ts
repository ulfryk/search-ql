import { List } from 'immutable';
import { Expression, ExpressionType } from '../common/model';
import { FunctionConfig } from '../config';
import { IBinaryOperationExpression, IExpression, IFunctionExpression, INotExpression, IPhraseExpression, ISelectorExpression, ITermExpression } from '../dto';
import { BinaryOperationExpression, DateExpression, FunctionExpression, NotExpression, NumberExpression, PhraseExpression, SelectorExpression, TermExpression, TextExpression } from './expressions';
import { Operator } from './operators';

// tslint:disable-next-line:cyclomatic-complexity
export const fromJS = <E extends IExpression>(pojo: E): Expression => {

  switch (pojo.type) {
    case ExpressionType.Date: {
      const { value } = (pojo as any as ITermExpression);
      return new DateExpression(value);
    }

    case ExpressionType.Number: {
      const { value } = (pojo as any as ITermExpression);
      return new NumberExpression(value);
    }

    case ExpressionType.Text: {
      const { value } = (pojo as any as ITermExpression);
      return new TextExpression(value);
    }

    case ExpressionType.Selector: {
      const { matchingType, value } = (pojo as any as ISelectorExpression);
      return new SelectorExpression(matchingType, value);
    }

    case ExpressionType.Phrase: {
      const { term } = (pojo as any as IPhraseExpression);
      return new PhraseExpression(fromJS(term) as TermExpression);
    }

    case ExpressionType.Not: {
      const { value } = (pojo as any as INotExpression);
      return new NotExpression(fromJS(value));
    }

    case ExpressionType.Binary: {
      const { operator, value } = (pojo as any as IBinaryOperationExpression<any, any>);
      const [lhs, rhs] = value;
      return new BinaryOperationExpression(Operator.fromJS(operator),
        [fromJS(lhs), fromJS(rhs)]);
    }

    case ExpressionType.Function: {
      const { config, value } = (pojo as any as IFunctionExpression);
      return new FunctionExpression(List(value.map(fromJS)), FunctionConfig.fromJS(config));
    }

    default: throw new Error(`No such expression type: "${pojo.type}"`);
  }

};
