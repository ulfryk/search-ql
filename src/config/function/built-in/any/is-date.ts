import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const isDateFunction =
  new FunctionConfig(
    'is_date',
    List([RequiredFunctionArg.fromType(ValueType.Any, 'value')]),
    None(),
    Right(ValueType.Boolean));
