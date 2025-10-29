package com.simply.ai.server.inner.service;

import java.math.BigInteger;

public interface UserService {
    public BigInteger authorizationAndGetUserId(String authorization);
}
