package com.simply.ai.server.manager.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ChatgptResponse {

    // 事件类型，如response.created, response.in_progress, response.completed等
    private String type;

    // 序列号
    @JsonProperty("sequence_number")
    private Integer sequenceNumber;

    // 响应对象，对于类型为response.xxx的事件
    private Response response;

    // 以下字段用于response.content_part.delta等事件
    @JsonProperty("item_id")
    private String itemId;

    @JsonProperty("output_index")
    private Integer outputIndex;

    @JsonProperty("content_index")
    private Integer contentIndex;

    // 增量文本，用于response.output_text.delta
    private String delta;

    // 完整文本，用于response.output_text.done
    private String text;

    // 用于response.content_part.done等事件
    private ContentPart part;

    // 用于response.output_item.added等事件
    private OutputItem item;

    // 内部类，对应响应对象
    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class Response {
        private String id;
        private String object;
        @JsonProperty("created_at")
        private Long createdAt;
        private String status;
        private String model;
        private List<OutputItem> output;
        private Usage usage;
        // 其他字段根据需要添加
    }

    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class OutputItem {
        private String id;
        private String type;
        private String status;
        private List<ContentPart> content;
        private String role;
    }

    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ContentPart {
        private String type;
        private String text;
        private List<Object> annotations;
        private List<Object> logprobs;
    }

    @Data
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class Usage {
        @JsonProperty("input_tokens")
        private Integer inputTokens;
        @JsonProperty("output_tokens")
        private Integer outputTokens;
        @JsonProperty("total_tokens")
        private Integer totalTokens;
        @JsonProperty("input_tokens_details")
        private Map<String, Object> inputTokensDetails;
        @JsonProperty("output_tokens_details")
        private Map<String, Object> outputTokensDetails;
    }
}