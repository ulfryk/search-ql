import { Either, Left, Right } from 'monet';

import { Expression, TypeFailure } from '../../common/model';
import { InvalidExpression } from './invalid';

export const validate = (e: Expression): Either<TypeFailure[], Expression> => {
  const validated = e.checkTypes();

  if (validated.isValid()) {
    return Right(e);
  }

  return Left(validated.toList()
    .filter(node => node instanceof InvalidExpression)
    .flatMap(({ errors }: InvalidExpression) => errors.map(TypeFailure.fromError(validated)))
    .toArray());
};
