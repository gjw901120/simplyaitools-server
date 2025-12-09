package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.web.model.dto.request.user.LoginByEmailDTO;
import com.simply.ai.server.web.model.dto.request.user.RegisterByEmailDTO;
import com.simply.ai.server.web.model.dto.request.user.SendEmailCodeDTO;
import com.simply.ai.server.web.service.UserService;
import org.springframework.stereotype.Service;


@Service
public class UserServiceImpl implements UserService {


    @Override
    public Boolean sendEmailCode(SendEmailCodeDTO request) {
        return true;
    }

    @Override
    public Boolean registerByEmail(RegisterByEmailDTO request) {
        return true;
    }

    @Override
    public Boolean loginByEmail(LoginByEmailDTO request) {
        return true;
    }

}
