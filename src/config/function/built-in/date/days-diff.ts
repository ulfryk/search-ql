import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const daysDiffFunction =
  new FunctionConfig(
    'days_diff',
    List([
      RequiredFunctionArg.fromType(ValueType.Date, 'end_date'),
      RequiredFunctionArg.fromType(ValueType.Date, 'start_date'),
    ]),
    None(),
    Right(ValueType.Number));
