package com.simply.ai.server.manager.model.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import java.util.List;

@Data
public class ClaudeRequest {
    private String model;
    private Integer max_tokens;
    private List<Message> messages;
    private Boolean stream = true;
    private List<Tool> tools;

    @Data
    public static class Message {
        private String role;
        private Object content; // 可以是String或List<ContentItem>

        // 构造函数
        public Message() {}

        public Message(String role, String content) {
            this.role = role;
            this.content = content;
        }
    }

    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class Tool {
        private String type;
        private String name;
        private Integer max_uses;

    }
}