import { Map } from 'immutable';
import { Either, Left, Right } from 'monet';

import { Expression, Failure, ValueType } from '../../common/model';
import { InvalidExpression } from './invalid';

export const validate = (model: Map<string, ValueType>) =>
  (e: Expression): Either<Failure[], Expression> => {
    const validated = e.checkTypes().checkIntegrity(model);

    if (validated.isValid()) {
      return Right(e);
    }

    return Left(validated.toList()
      .filter(node => node instanceof InvalidExpression)
      .flatMap(expr => (expr as InvalidExpression).errors)
      .toArray());
  };
