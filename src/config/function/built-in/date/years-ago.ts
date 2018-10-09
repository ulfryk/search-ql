import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const yearsAgoFunction =
  new FunctionConfig(
    'years_ago',
    List([RequiredFunctionArg.fromType(ValueType.Date, 'date')]),
    None(),
    Right(ValueType.Number));
