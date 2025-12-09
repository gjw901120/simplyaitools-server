package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.user.LoginByEmailDTO;
import com.simply.ai.server.web.model.dto.request.user.RegisterByEmailDTO;
import com.simply.ai.server.web.model.dto.request.user.SendEmailCodeDTO;

public interface UserService {

    Boolean sendEmailCode(SendEmailCodeDTO request);

    Boolean registerByEmail(RegisterByEmailDTO request);

    Boolean loginByEmail(LoginByEmailDTO  request);


}
