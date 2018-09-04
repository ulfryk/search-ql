import { Either, Left, Right } from 'monet';

import { TypeFailure } from '../../common/model';
import { Expression } from './expression';
import { InvalidExpression } from './invalid';

export const validate = (e: Expression): Either<TypeFailure[], Expression> => {
  const validated = e.checkTypes();

  if (validated.isValid()) {
    return Right(e);
  }

  return Left(validated.toList()
    .filter(node => node instanceof InvalidExpression)
    .flatMap(({ errors }: InvalidExpression) => errors.map(error => new TypeFailure(error)))
    .toArray());
};
