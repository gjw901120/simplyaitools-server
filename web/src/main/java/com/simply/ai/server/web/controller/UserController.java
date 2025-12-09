package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.model.dto.request.user.RegisterByEmailDTO;
import com.simply.ai.server.web.model.dto.request.user.SendEmailCodeDTO;
import com.simply.ai.server.web.service.UserService;
import com.simply.common.core.entity.vo.ResponseResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/user")
public class UserController {

    @Autowired
    private UserService userService;

    @PostMapping("/send-email-code")
    public ResponseResult<?> sendEmailCode(@RequestBody @Valid SendEmailCodeDTO request) {

        return ResponseResult.success(userService.sendEmailCode(request));

    }

    @PostMapping("/register-by-email")
    public ResponseResult<?> registerByEmail(@RequestBody @Valid RegisterByEmailDTO request) {

        return ResponseResult.success(userService.registerByEmail(request));

    }

}
